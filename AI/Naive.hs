{-# LANGUAGE RecordWildCards #-}
module AI.Naive where 
import Pyramid

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



