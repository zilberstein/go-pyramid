{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Pyramid where

import Cards

import Control.Monad
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Game = Game
  { gMiddle  :: [Card]
  , gDeck    :: [Card]
  , gDiscard :: [Card]
  , gP1      :: Player
  , gP2      :: Player
  }
  deriving (Show)

data Player = Player
  { pHand    :: [Card]
  , pPyramid :: [Card]
  }
  deriving (Show)

deal :: Int -> Int -> [Card] -> Maybe ([Card], [Card], [Card])
deal n m deck
  | n + m > length deck = Nothing
  | n >= m = go n m deck
  | otherwise = (\(p2,p1,d) -> (p1,p2,d)) <$> go m n deck
  where
    go n 0 d = Just (take n d, [], drop n d)
    go n m (c1 : c2 : d) = do
      (h1, h2, d') <- go (n-1) (m-1) d
      Just (c1:h1, c2:h2, d')
    go _ _ _ = Nothing

initGame :: IO Game
initGame = go <$> shuffle allCards
  where
    go deck = Game{..}
      where
        (gMiddle, deck') = splitAt 5 deck
        gDiscard = []
        Just (h1, h2, gDeck) = deal 5 5 deck'
        gP1 = Player (sort h1) []
        gP2 = Player (sort h2) []
        
type Pyramid = [Card]

hasCard :: Label -> Pyramid -> Bool
hasCard lbl = isJust . find (\(Card l _) -> l == lbl)

supported :: Label -> Pyramid -> Bool -> Bool
supported lbl p b =
  (if b then
     hasCard lbl p
   else
     not $ hasCard lbl p
  ) &&
  case lbl of
    Ten -> hasCard Nine p && hasCard Eight p
    Nine -> hasCard Seven p && hasCard Six p
    Eight -> hasCard Six p && hasCard Five p
    l -> fromEnum l < 4 || (hasCard (toEnum (fromEnum l - 4)) p && hasCard (toEnum (fromEnum l - 3)) p)

add :: Card -> Pyramid -> Maybe Pyramid
add c@(Card l _) p | supported l p False = Just $ c : p
                   | otherwise = Nothing

isComplete :: Pyramid -> Bool
isComplete p = supported Ten p True

renderIf :: Label -> [Card] -> String
renderIf lbl cs = maybe "      " show (find (\ (Card x _) -> x == lbl) cs)

renderLine :: Int -> [Label] -> [Card] -> String
renderLine n ls hand =
  concat (replicate n " ") ++ concat [ renderIf l hand | l <- ls ]

renderBoard :: [Card] -> [Card] -> String
renderBoard p1 p2 = unlines $ intersperse " "
    [ renderLine 12 [Ten] p1
    , renderLine 9 [Eight, Nine] p1
    , renderLine 6 [Five .. Seven] p1
    , renderLine 3 [Ace .. Four] p1
    , unwords $ replicate 5 "\ESC[107;30m  Z  \ESC[0m"
    , renderLine 3 [Ace .. Four] p2
    , renderLine 6 [Five .. Seven] p2
    , renderLine 9 [Eight, Nine] p2
    , renderLine 12 [Ten] p2
    ]

renderHand :: [Card] -> String
renderHand = unwords . map show

renderGame :: Game -> String
renderGame Game{..} = unlines
  [ "P1: " ++ renderHand (pHand gP1)
  , ""
  , renderBoard (pPyramid gP1) (pPyramid gP2)
  , ""
  , "P2: " ++ renderHand (pHand gP2)
  ]

-- GAME PLAY -------------------------------------------------------------------

data Move = SwapMiddle | Do [Action]
    deriving Show 
data Action = Put | Throw | Keep
    deriving Show 

type Strategy m = Player -> m Move

doActions :: Player -> [Action] -> Maybe (Player, [Card])
doActions Player{..} acts = do
  (h, p, d) <- go (zip pHand acts) [] pPyramid []
  Just (Player { pHand = h, pPyramid = p }, d)
  where
    go :: [(Card, Action)] -> [Card] -> [Card] -> [Card] -> Maybe ([Card], [Card], [Card])
    go ((c, a) : tl) h pyr d = case a of
      Put   -> (\p -> go tl h p d) =<< add c pyr
      Throw -> go tl h pyr (c : d)
      Keep  -> go tl (c : h) pyr d
    go [] h pyr d = Just (h, pyr, d)

doRound :: Game -> Strategy IO -> Strategy IO -> IO Game
doRound g s1 s2 = do
  m1 <- s1 (gP1 g)
  let g2 =
        case m1 of
          SwapMiddle -> g { gMiddle = pHand $ gP1 g
                          , gP1     = Player (gMiddle g) (pPyramid $ gP1 g)
                          }
          Do acts | Just (p, d) <- doActions (gP1 g) acts ->
                      g { gP1 = p
                        , gDiscard = d ++ gDiscard g
                        }
                  | otherwise -> g
  m2 <- s2 (gP2 g)
  let g3 =
        case m2 of
          SwapMiddle -> g2 { gMiddle = pHand $ gP2 g2
                           , gP2     = Player (gMiddle g2) (pPyramid $ gP2 g2)
                           }
          Do acts | Just (p, d) <- doActions (gP2 g2) acts ->
                      g2 { gP2 = p
                         , gDiscard = d ++ gDiscard g2
                         }
                  | otherwise -> g2
  
  let n = 5 - length (pHand $ gP1 g3)
      m = 5 - length (pHand $ gP2 g3)
      goDeal gm@Game{..} = case deal n m gDeck of
        Just (h1, h2, d) -> return gm { gDeck = d
                                      , gP1 = gP1 { pHand = sort $ pHand gP1 ++ h1 }
                                      , gP2 = gP2 { pHand = sort $ pHand gP2 ++ h2 }
                                      }
        Nothing -> do
          d <- shuffle gDiscard
          goDeal $ gm { gDeck = gDeck ++ d, gDiscard = [] }
  goDeal g3

data Outcome = P1Win | P2Win | Draw | Deadlock
  deriving (Show, Eq, Ord)

hasWon :: Player -> Bool
hasWon Player{..} = supported Ten pPyramid True

gameLoop :: Strategy IO -> Strategy IO -> Int -> Game -> IO Outcome
gameLoop s1 s2 n g@Game{..}
  | n <= 0 = return Deadlock
  | otherwise = do
--      putStrLn $ renderGame g
      let p1win = hasWon gP1
          p2win = hasWon gP2
      if p1win && p2win
        then
        return Draw
        else if p1win then
        return P1Win
        else if p2win then
        return P2Win
        else 
          gameLoop s1 s2 (n-1) =<< doRound g s1 s2

recordStats :: Int -> Strategy IO -> Strategy IO -> IO (Map Outcome Int)
recordStats n s1 s2 = Map.fromListWith (+) . map (,1) <$> replicateM n (gameLoop s1 s2 100 =<< initGame)
