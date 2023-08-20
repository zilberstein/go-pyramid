{-# LANGUAGE RecordWildCards #-}
module AI.Naive where 

import Cards
import Pyramid

import System.Random

strategy :: Monad m => Strategy m 
strategy Player{..} = 
    
    return $ Do $ reverse (getActions pHand (pPyramid, []))
        where 
            getActions (card:cards) (pPyramid, list) = getActions cards (tryInsert card pPyramid list)
            getActions [] (pPyramid, list) = list
            tryInsert card pPyramid actions =
                        case add card pPyramid of
                            Just pyramid -> (pyramid, Put:actions)
                            Nothing -> (pPyramid, Throw:actions)

hoard :: Monad m => Label -> Strategy m 
hoard lbl Player{..} = 
    return $ Do $ reverse (getActions pHand (pPyramid, []))
        where
            getActions (card:cards) (pPyramid, list) = getActions cards (tryInsert card pPyramid list)
            getActions [] (pPyramid, list) = list
            tryInsert card pPyramid actions =
                        case add card pPyramid of
                            Just pyramid -> (pyramid, Put:actions)
                            Nothing
                              | Card l _ <- card, l == lbl -> (pPyramid, Keep:actions)
                              | otherwise -> (pPyramid, Throw:actions)



