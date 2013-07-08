module Main (
  main
) where

import Data.List

import System.Environment
import System.IO

import LJI.Args (Options(..), parseArgs)
import LJI.Console (putStringAndExit)
import LJI.Run (run, runEnvironment)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  args <- intercalate " " `fmap` getArgs
  eOpts <- parseArgs args
  case eOpts of 
    Left  e  -> putStringAndExit e 
    Right os -> flip runEnvironment run (os { optArgString = args })
