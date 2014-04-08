
{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Markov
  (
    Store
  , RNG
  , analyse
  , genSentence
  , genSentences
  ) where

import Control.Arrow       ((&&&))
import Control.Monad.State (State, state)
import Data.Bitraversable  (bisequence)
import Data.Char           (isUpper)
import Data.List           (unfoldr)
import Data.Text           (Text)
import System.Random

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map as M

type Word = T.Text

type Store = M.Map [Word] (S.Set Word)

analyse :: Int -> Text -> Store
analyse n = store . slidingN n . parse

isSentenceStart :: Word -> Bool
isSentenceStart = isUpper . T.head

isSentenceEnd :: Word -> Bool
isSentenceEnd w = case T.last w of
  '.' -> True
  '!' -> True
  '?' -> True
  _   -> False

parse :: T.Text -> [Word]
parse = T.words

slidingN :: Int -> [a] -> [[a]]
slidingN n = unfoldr $ bisequence . (take' n &&& Just . drop 1)

take' :: Int -> [a] -> Maybe [a]
take' 0 _ = Just []
take' n [] | n > 0     = Nothing
           | otherwise = Just []
take' n (x:xs) = fmap (x:) $ take' (n-1) xs


store :: [[Word]] -> Store
store = M.fromListWith S.union . map (init &&& S.singleton . last)

genSentences :: Int -> Int -> Store -> RNG [Word]
genSentences n minWs st =
  fmap (takeSentences . concat) $ sequence . repeat $ genSentence n st
    where takeSentences ws = let (ws', ws'') = splitAt (minWs - 1) ws
                             in ws' ++ takeUntil isSentenceEnd ws''

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x = [x]
                   | otherwise = x : takeUntil p xs

genSentence :: Int -> Store -> RNG [Word]
genSentence n st = fmap reverse $ rest . reverse =<< firstWords st
  where rest ws @ (w:_)
          | isSentenceEnd w = return ws
          | otherwise = do w' <- nextWord st $ reverse $ take (n-1) ws
                           rest (w':ws)

firstWords :: Store -> RNG [Word]
firstWords = randElem . filter (isSentenceStart . head) . M.keys

nextWord :: Store -> [Word] -> RNG Word
nextWord st k = randElem $ S.elems candidates
  where candidates = st M.! k


-- Random number generation

type RNG a = (RandomGen g) => State g a

randElem :: [a] -> RNG a
randElem xs = fmap (xs !!) $ nextInt (length xs)

nextInt :: Int -> RNG Int
nextInt ceil = fmap (`mod` ceil) $ state next
