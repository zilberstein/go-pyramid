{-# LANGUAGE RecordWildCards #-}
module AI.Naive where 

import Cards
import Pyramid

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
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

keepNextRow :: Monad m => Strategy m
keepNextRow Player{..} _ _ = return $ Do $ getActs [] [] pPyramid pHand
  where
    getActs _ moves _ [] = reverse moves
    getActs keeps moves pyr (card@(Card l _) : cs)
      | Just pyr' <- add card pyr = getActs keeps (Put : moves) pyr' cs
      | needSoon l keeps pyr = getActs (l : keeps) (Keep : moves) pyr cs
      | otherwise = getActs keeps (Throw : moves) pyr cs
    needSoon lbl keeps pyr =
      let has l = supported l pyr True in
        not (elem lbl keeps || has lbl) &&
        case lbl of
          Five  -> has Ace || has Two
          Six   -> has Two || has Three
          Seven -> has Three || has Four
          Eight -> has Five || has Six
          Nine  -> has Six || has Seven
          Ten   -> has Eight || has Nine
          _ -> False

-- USE MIDDLE ------------------------------------------------------------------

data PNum = P1 | P2

numPlayable :: [Card] -> [Card] -> Int
numPlayable = go 0
  where
    go n [] _ = n
    go n (card : cards) pyr 
      | Just pyr' <- add card pyr = go (n+1) cards pyr'
      | otherwise = go n cards pyr

numNotIn :: [Card] -> [Card] -> Int
numNotIn hand pyr =
  sum [ 1
      | Card lbl _ <- hand
      , lbl < Jack
      , lbl `notElem` [ l | Card l _ <- pyr ]
      ]

initKeepNextWithMiddle :: PNum -> IO (Strategy IO)
initKeepNextWithMiddle pnum = do
  ref <- newIORef Nothing
  return $ \p@Player{..} opPyr opLastMove -> do
    -- UPDATE STATE
    when (opLastMove == Just SwapMiddle) $ writeIORef ref Nothing
    -- GET NAIVE MOVE
    m <- keepNextRow p opPyr opLastMove
    -- DECIDE WHETHER TO SWAP MIDDLE
    middle <- readIORef ref
    case middle of
      Just hand
        -- Take the middle if it's better than the current hand
        | numPlayable hand pPyramid > numPlayable pHand pPyramid -> do
            writeIORef ref $ Just pHand
            return SwapMiddle
        -- If there are no cards to put, then stash the hand if it's better than
        -- what's in the middle
        | Do acts <- m
        , Put `notElem` acts
        , numNotIn hand pPyramid < numNotIn pHand pPyramid -> do
            writeIORef ref $ Just pHand
            return SwapMiddle
      Nothing
        | Do acts <- m
        , Put `notElem` acts
        , numNotIn pHand pPyramid > 2 -> do
            writeIORef ref $ Just pHand
            return SwapMiddle
      _ -> return m
    
