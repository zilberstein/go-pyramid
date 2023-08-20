{-# LANGUAGE RecordWildCards #-}
module AI.Naive where 

import Cards
import Pyramid

import System.Random

strategy :: Monad m => Strategy m 
strategy = keepIf $ const False

hoard :: Monad m => Label -> Strategy m 
hoard lbl = keepIf $ \(Card l _) -> l == lbl

keepIf :: Monad m => (Card -> Bool) -> Strategy m
keepIf f Player{..} _ _ = return $ Do $ reverse (getActions pHand (pPyramid, []))
  where
    getActions (card:cards) (pPyramid, list) = getActions cards (tryInsert card pPyramid list)
    getActions [] (pPyramid, list) = list
    tryInsert card pPyramid actions =
      case add card pPyramid of
        Just pyramid -> (pyramid, Put:actions)
        Nothing
          | f card    -> (pPyramid, Keep:actions)
          | otherwise -> (pPyramid, Throw:actions)
