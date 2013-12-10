{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Control.Arrow       ((&&&))
import Control.Monad.State (State, state, evalState)
import Data.Bitraversable  (bisequence)
import Data.Char           (isUpper)
import Data.Foldable       (foldMap)
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

genSentence :: Int -> Store -> RNG [Word]
genSentence n st = do
  fws <- firstWords st
  ws  <- genSentence' $ reverse fws
  return $ reverse ws  
    where genSentence' ws @ (w:ws')
            | isSentenceEnd w = return ws
            | otherwise = do w' <- nextWord st $ reverse $ take (n-1) ws
                             genSentence' (w':ws)

firstWords :: Store -> RNG [Word]
firstWords st = randElem candidates
  where candidates = filter suitable (M.keys st)
        suitable   = liftA2 (&&) isSentenceStart (not . isSentenceEnd) . head

nextWord :: Store -> [Word] -> RNG Word
nextWord st k = randElem $ S.elems candidates
  where candidates = st M.! k

nextInt :: Int -> RNG Int
nextInt max = (`mod` max) <$> state next

randElem :: [a] -> RNG a
randElem xs = (xs !!) <$> nextInt (length xs)

data Options = Opts { chainLength :: Int, inputFiles :: [FilePath] }

opts :: Parser Options
opts = Opts
  <$> option (long "chain-length"
           <> short 'n'
           <> value 3
           <> metavar "NUMBER")
  <*> some (argument str $ metavar "FILES...")

run :: Options -> IO ()
run (Opts n files) = do
  contents <- traverse TIO.readFile files
  gen      <- getStdGen
  let stored = store . slidingN n . parse . T.concat $ contents  
  let sentence = evalState (genSentence n stored) gen
  TIO.putStrLn . T.concat . intersperse " " $ sentence

main :: IO ()
main = execParser optsInfo >>= run
  where optsInfo = info (helper <*> opts)
          (fullDesc <> progDesc "Generate a sentence based on input text")
