{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Control.Monad.State (State, state, evalState)
import Data.Char           (isUpper)
import Data.Functor        ((<$>))
import Data.Monoid         (Monoid(..))
import Data.Foldable       (foldMap)
import Data.Traversable    (traverse)
import Data.List           (intersperse)
import Data.Set            (Set)
import System.Environment  (getArgs)
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

sliding3 :: [a] -> [(a, a, a)]
sliding3 xs = zip3 xs xs1 xs2
  where xs1 = drop 1 xs
        xs2 = drop 1 xs1

type Store = SMap (Word, Word) (Set Word)

store :: [(Word, Word, Word)] -> Store
store = foldMap $ \(w1, w2, w3) ->
  SMap $ Map.singleton (w1, w2) (Set.singleton w3)

type RNG a = (RandomGen g) => State g a

genSentence :: Store -> RNG [Word]
genSentence st = do
  (w1, w2) <- firstWords st
  ws <- genSentence' [w2, w1]
  return $ reverse ws  
    where genSentence' ws @ (w2:w1:_)
            | isSentenceEnd w2 = return ws
            | otherwise = do w3 <- nextWord st (w1, w2)
                             genSentence' (w3:ws)

firstWords :: Store -> RNG (Word, Word)
firstWords (SMap st) = randElem candidates
  where candidates = filter (isSentenceStart . fst) (Map.keys st)

nextWord :: Store -> (Word, Word) -> RNG Word
nextWord (SMap st) k = randElem $ Set.elems candidates
  where candidates = st Map.! k

nextInt :: Int -> RNG Int
nextInt max = (`mod` max) <$> state next

randElem :: [a] -> RNG a
randElem xs = (xs !!) <$> nextInt (length xs)

main :: IO ()
main = do
  files <- getArgs
  contents <- traverse TIO.readFile files
  gen <- getStdGen
  let stored = store . sliding3 . parse . T.concat $ contents
  let sentence = evalState (genSentence stored) gen
  TIO.putStrLn . T.concat . intersperse " " $ sentence
