{-# LANGUAGE OverloadedStrings #-}

module Main where

import Markov

import Control.Monad.State (evalState)
import Data.List           (intersperse)
import Data.Traversable    (traverse)
import Options.Applicative
import System.Random       (getStdGen)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
  let markov = analyse n $ T.concat contents
  let sentence = evalState (genSentences n m markov) gen
  TIO.putStrLn . T.concat . intersperse " " $ sentence

main :: IO ()
main = execParser optsInfo >>= run
  where optsInfo = info (helper <*> opts)
          (fullDesc <> progDesc "Generate sentences based on input text")
