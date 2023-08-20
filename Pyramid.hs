{-# LANGUAGE RecordWildCards #-}
module Pyramid
  (
  ) where

import Cards

import Data.List
import Data.Maybe

data Game = Game
  { gMiddle :: [Card]
  , gDeck   :: [Card]
  , gP1     :: Player
  , gP2     :: Player
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
      (h1, h2, d') <- go (n-1) (n-1) d
      Just (c1:h1, c2:h2, d')
    go _ _ _ = Nothing

initGame :: IO Game
initGame = go <$> shuffle allCards
  where
    go deck = Game{..}
      where
        (gMiddle, deck') = splitAt 5 deck
        Just (h1, h2, gDeck) = deal 5 5 deck'
        gP1 = Player h1 []
        gP2 = Player h2 []
        

type Pyramid = [Card]

supported :: Card -> Pyramid -> Bool -> Bool
supported (Card lbl _) p b =
  (if b then
     isJust (find (\(Card l _) -> l == lbl) p)
   else
     isNothing (find (\(Card l _) -> l == lbl) p)
  ) &&
  case lbl of
    Ten -> sup Nine && sup Eight
    Nine -> sup Seven && sup Six
    Eight -> sup Six && sup Five
    l -> fromEnum l < 4 || (sup (toEnum (fromEnum l - 4)) && sup (toEnum (fromEnum l - 3)))
    where
      sup l = supported (Card l Heart) p True

add :: Card -> Pyramid -> Maybe Pyramid
add c p | supported c p False = Just $ c : p
        | otherwise = Nothing

isComplete :: Pyramid -> Bool
isComplete p = supported (Card Ten Spade) p True



renderIf :: Label -> [Card] -> String
renderIf lbl cs = case find (\(Card x _) -> x == lbl) cs of
  Just c -> show c
  Nothing -> "     "

renderLine :: Int -> [Label] -> [Card] -> String
renderLine n ls hand =
  concat (replicate n " ") ++ intercalate " " [ renderIf l hand | l <- ls ]

renderBoard :: [Card] -> [Card] -> String
renderBoard p1 p2 = unlines $ intersperse " "
    [ renderLine 12 [Ten] p1
    , renderLine 9 [Eight, Nine] p1
    , renderLine 6 [Five .. Seven] p1
    , renderLine 3 [Ace .. Four] p1
    , intercalate " " $ replicate 5 "\ESC[107;30m  Z  \ESC[0m"
    , renderLine 3 [Ace .. Four] p2
    , renderLine 6 [Five .. Seven] p2
    , renderLine 9 [Eight, Nine] p2
    , renderLine 12 [Ten] p2
    ]

-- GAME PLAY -------------------------------------------------------------------

data Move = SwapMiddle | Do [Action]
data Action = Put | Throw | None

type Strategy m = [Card] -> [Card] -> m Move

doRound :: Monad m => Game -> Strategy m -> Strategy m -> m Game
doRound Game{..} s1 s2 = do
  undefined
