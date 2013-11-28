{-# LANGUAGE OverloadedStrings #-}

module Markov where

import Control.Monad.State
import Data.Char          (isUpper)
import Data.Functor       ((<$>))
import Data.Monoid
import Data.Foldable      (foldMap)
import Data.Traversable   (traverse)
import Data.List          (intersperse)
import Data.Set           (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Data.Text          (Text)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

-- | Newtype for a Map with a more useful Monoid
newtype SMap a b = SMap { runSMap :: Map.Map a b } deriving Show

instance (Ord a, Monoid b) => Monoid (SMap a b) where
  mempty = SMap Map.empty
  mappend (SMap m) = SMap . Map.unionWith mappend m . runSMap

singleton :: a -> b -> SMap a b
singleton a b = SMap $ Map.singleton a b

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

seqs3 :: [Word] -> [(Word, Word, Word)]
seqs3 ws = zip3 ws ws1 ws2
  where ws1 = drop 1 ws
        ws2 = drop 1 ws1

type Store = SMap (Word, Word) (Set Word)

store :: [(Word, Word, Word)] -> Store
store = foldMap toMap
  where toMap (w1, w2, w3) = singleton (w1, w2) $ Set.singleton w3

genSentence :: (RandomGen g) => Store -> State g [Word]
genSentence st = do
  (w1, w2) <- firstWords st
  ws <- genSentence' [w2, w1]
  return $ reverse ws  
    where
      genSentence' ws @ (w2:w1:_)
        | isSentenceEnd w2 = return ws
        | otherwise = do
                      w3 <- nextWord st (w1, w2)
                      genSentence' (w3:ws)

firstWords :: (RandomGen g) => Store -> State g (Word, Word)
firstWords (SMap st) = randElem candidates
  where candidates = filter (isSentenceStart . fst) (Map.keys st)

nextWord :: (RandomGen g) => Store -> (Word, Word) -> State g Word
nextWord (SMap st) k = randElem $ Set.elems candidates
  where candidates = st Map.! k

nextInt :: (RandomGen g) => Int -> State g Int
nextInt max = do gen <- get
                 let (i, gen') = next gen
                 put gen'
                 return $ i `mod` max

randElem :: (RandomGen g) => [a] -> State g a
randElem xs = (xs !!) <$> nextInt (length xs)

main :: IO ()
main = do
  files <- getArgs
  contents <- TIO.readFile `traverse` files
  let stored = store . seqs3 . parse $ T.concat contents
  gen <- getStdGen
  TIO.putStrLn . T.concat . intersperse " " $ evalState (genSentence stored) gen
