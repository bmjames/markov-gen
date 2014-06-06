{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import Markov

import Control.Monad       (replicateM, void)
import Control.Monad.State (evalState)

import Data.List           (intersperse)
import Data.Traversable    (traverse)

import Options.Applicative

import System.Random       (newStdGen)

import qualified Data.Text    as T
import qualified Data.Text.IO as T


data Options = Opts { chainLength :: Int
                    , minWords :: Int
                    , sentences :: Int
                    , inputFiles :: [FilePath] }

opts :: Parser Options
opts = Opts
  <$> option (long "chain-length"
           <> short 'c'
           <> value 3
           <> metavar "NUMBER")
  <*> option (long "min-words"
           <> short 'm'
           <> value 1
           <> metavar "NUMBER")
  <*> option (long "sentences"
           <> short 'n'
           <> metavar "NUMBER")
  <*> some (argument str $ metavar "FILES...")

run :: Options -> IO ()
run (Opts c m n files) = do
  contents <- traverse T.readFile files
  let !markov = analyse c $ T.concat contents
  void . replicateM n $ do
    gen <- newStdGen
    let sentence = evalState (genSentences c m markov) gen
    T.putStrLn . T.concat . intersperse " " $ sentence

main :: IO ()
main = execParser optsInfo >>= run
  where optsInfo = info (helper <*> opts)
          (fullDesc <> progDesc "Generate sentences based on input text")
