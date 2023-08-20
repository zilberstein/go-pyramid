{-# LANGUAGE RecordWildCards #-}
module AI.Krewton where 

import Cards hiding (Deck)
import Pyramid
import AI.Naive

import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

data State = State
  { sDiscard  :: Deck
  , sDeck     :: Deck
  , sOpHand   :: Deck
  , sLastMove :: Maybe ([Card], Move)
  , sOpPyr    :: [Card]
  }
  deriving Show

data Deck = Deck
  { dCards :: Map Label (Int, Int)
  , dSize  :: Int
  }
  deriving Show

instance Semigroup Deck where
  d1 <> d2 = Deck
    { dCards = Map.unionWith addR (dCards d1) (dCards d2)
    , dSize  = dSize d1 + dSize d2
    }
instance Monoid Deck where
    mempty = Deck Map.empty 0

addR :: (Int, Int) -> (Int, Int) -> (Int, Int)
addR (l1, h1) (l2, h2) = (min 4 (l1+l2), min 4 (h1+h2))

freshDeck :: Deck
freshDeck = Deck
  { dCards = Map.fromList [(lbl, (4,4)) | lbl <- [Ace .. King]]
  , dSize  = 52
  }

initStrategy :: IO (Strategy IO)
initStrategy = do
  st <- newIORef State { sDiscard  = mempty
                       , sDeck     = freshDeck
                       , sOpHand   = mempty
                       , sLastMove = Nothing
                       , sOpPyr    = []
                       }
  return $ runStrategy st

cardsNeeded :: Maybe (Move, Move) -> Int
cardsNeeded Nothing = 15
cardsNeeded (Just (m1, m2)) = needed m1 + needed m2
  where
    needed (Do acts) = 5 - sum [ 1 | Keep <- acts]
    needed SwapMiddle = 0

getDiscards :: [Card] -> Move -> [Card]
getDiscards hand (Do acts) = [ c | (c, Throw) <- zip hand acts]
getDiscard hand SwapMiddle = []

addCard :: Card -> Deck -> Deck
addCard (Card lbl _) d@Deck{..}
  | Just (lo, hi) <- Map.lookup lbl dCards =
      Deck { dCards = Map.insert lbl (lo + 1, min 4 (hi+1)) dCards
           , dSize = dSize + 1 }
  | otherwise = Deck { dCards = Map.insert lbl (1,1) dCards, dSize = dSize + 1 }

addCards :: [Card] -> Deck -> Deck
addCards cs d = foldr addCard d cs

addN :: Int -> Deck -> Deck
addN n d =
  d <> Deck (Map.fromList [(lbl, (0,min 4 n)) | lbl <- [Ace .. King]]) n

removeCard :: Card -> Deck -> Deck
removeCard (Card lbl _) d@Deck{..}
  | Just (lo, hi) <- Map.lookup lbl dCards =
    Deck { dCards = Map.insert lbl (max 0 (lo-1), max 0 (hi-1)) dCards
         , dSize = dSize - 1 }
  | otherwise = d { dSize = dSize - 1 }

removeCards :: [Card] -> Deck -> Deck
removeCards cs d = foldr removeCard d cs

removeN :: Int -> Deck -> (Deck, Deck)
removeN n Deck{..} = (Deck c1 (dSize - n), Deck c2 n)
  where
    loSum = sum $ map fst $ Map.elems dCards
    hiSum = sum $ map snd $ Map.elems dCards
    (c1, c2) = Map.foldrWithKey f (Map.empty, Map.empty) dCards
    f lbl (lo, hi) (m1, m2) =
      (Map.insert lbl (l1, h1) m1, Map.insert lbl (l2, h2), m2)
      where
        ldiff = (loSum - lo + hi) - (dSize - n)
        hdiff = (hiSum - hi + lo) - (dSize - n)
        (l1, h2) | ldiff < 0 = (negate ldiff, hi + ldiff)
                 | otherwise = (0, hi)
        (l2, h1) | 
            () 
      | lowSum - lo + hi > dSize - n =
        ( Map.insert lbl (lowSum - lo + hi - (dSize - n), min 4 (min n hi))
        , Map.insert lbl ()
        )

  Deck { dCards = Map.map (\(lo, hi) -> (max 0 (lo-n), hi)) dCards
       , dSize  = dSize - n
       }

decrementCard :: Card -> Deck -> Deck
decrementCard (Card lbl _) d@Deck{..}
  | Just (lo, hi) <- Map.lookup lbl dCards =
    d { dCards = Map.insert lbl (lo, max lo (hi-1)) dCards }
  | otherwise = d

decrementCards :: [Card] -> Deck -> Deck
decrementCards cs d = foldr decrementCard d cs

updateState :: State -> Player -> Pyramid -> Maybe Move -> State
updateState oldst Player{..} opPyr opMove =
  updateDeal $ updateShuffle $ updateDiscard oldst
  where
    -- FIRST: Discard from previous round
    updateDiscard s@State{..} =
      s { sDiscard =
            addCards (case sLastMove of
                        Just (hand, mv) -> getDiscards hand mv
                        Nothing -> []) $
            addN (case opMove of
                    Just (Do acts) -> sum [ 1 | Throw <- acts]
                    _ -> 0)
            sDiscard
        , sDeck = decrementCards (opPyr \\ sOpPyr) sDeck
        , sOpPyr = opPyr
        }
    -- SECOND: Shuffle deck if needed
    updateShuffle s@State{..}
      | cardsNeeded ((,) . snd <$> sLastMove <*> opMove) > dSize sDeck =
          s { sDeck     = sDeck <> sDiscard
            , sDiscard  = mempty
            }
      | otherwise = s
    -- THIRD: deal new cards
    updateDeal s@State{..} = s { sDeck = removeCards myCards $ removeN opCards sDeck }
      where
        myCards = case sLastMove of
          Just (hand, Do acts) -> pHand \\ [ c | (c, Keep) <- zip hand acts]
          Just (_, SwapMiddle) -> []
          Nothing -> pHand
        opCards = case opMove of
          Just (Do acts) -> 5 - sum [ 1 | Keep <- acts ]
          Just SwapMiddle -> 0
          Nothing -> 5
      
    

runStrategy :: IORef State -> Strategy IO
runStrategy ref p@Player{..} opPyr opLastMove = do
    oldst <- readIORef ref
    let newst = updateState oldst p opPyr opLastMove
    writeIORef ref newst
    m <- strategy p opPyr opLastMove
    modifyIORef ref $ \s -> s { sLastMove = Just (pHand, m) }
    return m
