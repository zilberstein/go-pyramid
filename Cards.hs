module Cards ( Deck(..)
             , Card(..)
             , Suit(..)
             , Label(..)
             , suits
             , labels
             , allCards, noFace
             , shuffle) where

import qualified Data.Vector as Vector

import Control.Monad hiding (filterM, mapM, liftM)
import System.Random

type Deck = [Card]

-- The Card data type
data Card  = Card Label Suit

instance Eq Card where
    Card x _ == Card y _ = x == y

instance Ord Card where
    compare (Card x _) (Card y _) = compare x y

instance Show Card where
    show (Card l s) = show l ++ show s

data Suit  = Spade | Heart | Club | Diamond
             deriving (Eq)

instance Show Suit where
    show Spade   = "\ESC[107;30m\9824 \ESC[0m "
    show Heart   = "\ESC[107;31m\9829 \ESC[0m "
    show Club    = "\ESC[107;30m\9827 \ESC[0m "
    show Diamond = "\ESC[107;31m\9830 \ESC[0m "

data Label = Ace | Two   | Three | Four  | Five  | Six   | Seven
           | Eight | Nine  | Ten   | Jack  | Queen | King
             deriving (Eq, Ord, Enum)

instance Show Label where
    show l = "\ESC[107;30m " ++ show' l ++ space  ++ "\ESC[0m"
        where
          space | l /= Ten  = " "
                | otherwise = ""
          show' Jack  = "J"
          show' Queen = "Q"
          show' King  = "K"
          show' Ace   = "A"
          show' lbl   = show $ fromEnum lbl + 1

suits :: [Suit]
suits = [Spade, Heart, Club, Diamond]

labels :: [Label]
labels = [Ace .. King]

allCards :: [Card]
allCards =
  [ Card lbl suit
  | suit <- suits
  , lbl  <- labels
  ]

noFace :: [Card]
noFace =
  [ Card lbl suit
  | suit <- suits
  , lbl <- [Ace .. Ten]
  ]

shuffle :: [a] -> IO [a]
shuffle l = Vector.toList <$> foldM doSwap vec (reverse [1..Vector.length vec - 1]) 
    where
      vec = Vector.fromList l
      doSwap v i = do
        j <- randomRIO (0, i)
        let x = v Vector.! i
            y = v Vector.! j
        return $ v Vector.// [(i, y), (j, x)]
