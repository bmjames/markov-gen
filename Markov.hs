{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Control.Arrow       ((&&&))
import Control.Monad.State (State, state, evalState)
import Data.Bitraversable  (bisequence)
import Data.Char           (isUpper)
import Data.Traversable    (traverse)
import Data.List           (intersperse, unfoldr)
import Options.Applicative
import System.Random
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

type Word = T.Text

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
take' n (x:xs) = (x:) <$> take' (n-1) xs

type Store = M.Map [Word] (S.Set Word)

store :: [[Word]] -> Store
store = M.fromListWith S.union . map (init &&& S.singleton . last)

type RNG a = (RandomGen g) => State g a

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

randElem :: [a] -> RNG a
randElem xs = (xs !!) <$> nextInt (length xs)
  where nextInt ceil = flip mod ceil <$> state next

data Options = Opts { chainLength :: Int
                    , minWords :: Int
                    , inputFiles :: [FilePath] }

opts :: Parser Options
opts = Opts
  <$> option (long "chain-length"
           <> short 'n'
           <> value 3
           <> metavar "NUMBER")
  <*> option (long "min-words"
           <> short 'm'
           <> value 1
           <> metavar "NUMBER")
  <*> some (argument str $ metavar "FILES...")

run :: Options -> IO ()
run (Opts n m files) = do
  contents <- traverse TIO.readFile files
  gen      <- getStdGen
  let stored = store . slidingN n . parse . T.concat $ contents  
  let sentence = evalState (genSentences n m stored) gen
  TIO.putStrLn . T.concat . intersperse " " $ sentence

main :: IO ()
main = execParser optsInfo >>= run
  where optsInfo = info (helper <*> opts)
          (fullDesc <> progDesc "Generate nonsense sentences based on input text")
