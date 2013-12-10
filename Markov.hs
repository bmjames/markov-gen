{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Control.Arrow       ((&&&))
import Control.Monad.State (State, state, evalState)
import Data.Bitraversable  (bisequence)
import Data.Char           (isUpper)
import Data.Monoid         (Monoid(..))
import Data.Foldable       (foldMap)
import Data.Traversable    (traverse)
import Data.List           (intersperse, unfoldr)
import Data.Set            (Set)
import Options.Applicative
import Data.Text           (Text)
import System.Random
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

-- | Newtype for a Map with a more useful Monoid
newtype SMap a b = SMap (Map.Map a b) deriving Show

-- TODO this should only require Semigroup, not Monoid, for b
instance (Ord a, Monoid b) => Monoid (SMap a b) where
  mempty = SMap Map.empty
  mappend (SMap m1) (SMap m2) = SMap $ Map.unionWith mappend m1 m2

type Word = Text

isSentenceStart :: Word -> Bool

isSentenceStart = isUpper . T.head

isSentenceEnd :: Word -> Bool
isSentenceEnd w = case T.last w of
  '.' -> True
  '!' -> True
  '?' -> True
  _   -> False

parse :: Text -> [Word]
parse = T.words

slidingN :: Int -> [a] -> [[a]]
slidingN n = unfoldr $ bisequence . (take' n &&& Just . drop 1)

take' :: Int -> [a] -> Maybe [a]
take' 0 _ = Just []
take' n [] | n > 0     = Nothing
           | otherwise = Just []
take' n (x:xs) = (x:) <$> take' (n-1) xs

type Store = SMap [Word] (Set Word)

store :: [[Word]] -> Store
store = foldMap $ \ws ->
  SMap $ Map.singleton (init ws) (Set.singleton $ last ws)

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
firstWords (SMap st) = randElem candidates
  where candidates = filter suitable (Map.keys st)
        suitable   = liftA2 (&&) isSentenceStart (not . isSentenceEnd) . head

nextWord :: Store -> [Word] -> RNG Word
nextWord (SMap st) k = randElem $ Set.elems candidates
  where candidates = st Map.! k

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
