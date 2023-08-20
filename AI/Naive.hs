{-# LANGUAGE RecordWildCards #-}
module AI.Naive where 

import Cards
import Pyramid

import System.Random

strategy :: Monad m => Strategy m 
strategy = keepIf $ const $ return False

hoard :: Monad m => Label -> Strategy m 
hoard lbl = keepIf $ \(Card l _) -> return $ l == lbl

keepIf :: Monad m => (Card -> m Bool) -> Strategy m
keepIf f Player{..} _ _ = Do <$> getActions pHand (pPyramid, [])
  where
    getActions (card:cards) (pPyramid, list) = getActions cards =<< tryInsert card pPyramid list
    getActions [] (pPyramid, list) = return $ reverse list
    tryInsert card pPyramid actions =
      case add card pPyramid of
        Just pyramid -> return (pyramid, Put:actions)
        Nothing -> do
          keep <- f card
          if keep then
            return (pPyramid, Keep:actions)
            else
            return (pPyramid, Throw:actions)
