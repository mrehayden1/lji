module Main where

import Data.List
import Data.Maybe

import Debug.Trace

import System.Environment
import System.FilePath
import System.IO

import LJI.Args (Options(..), parseArgs)
import LJI.Console (putStringAndExit)
import LJI.Music.Composer (rootProgression, fifths, rootsFifths1, rootsFifths2,
         guideToneLine1, guideToneLine2)
import LJI.Run (run, runEnvironment)

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  args <- intercalate " " `fmap` getArgs
  -- TODO Use the standard parser for now despite arguments (output filename, --target) being ignored
  eOpts <- parseArgs args
  case eOpts of
    Left  e  -> putStringAndExit e 
    Right os -> runOptions os

runOptions :: Options -> IO ()
runOptions os =
 mapM_ (flip runEnvironment run) options
  where
    options :: [Options]
    options = do
      (t, tNm) <- [(Nothing           , Nothing  )
                  ,(Just [-1]         , Just "1a"), (Just [ 1]         , Just "1b")
                  ,(Just [-1, 1]      , Just "2a"), (Just [ 1,-1]      , Just "2b")
                  ,(Just [-1, 2, 1,-1], Just "3a"), (Just [ 1,-2,-1, 1], Just "3b")
                  ,(Just [-2,-1, 2, 1], Just "4a"), (Just [ 2, 1,-2,-1], Just "4b")
                  ,(Just [-3,-2,-1, 1], Just "5a"), (Just [ 3, 2, 1,-1], Just "5b")]
      (mFn, mFnNm) <- [ (rootProgression, Just "root"      )
                      , (fifths         , Just "fifth"     )
                      , (rootsFifths1   , Just "rootfifth1")
                      , (rootsFifths2   , Just "rootfifth2")
                      , (fifths         , Just "fifth"     )
                      , (guideToneLine1 , Just "guide1"    )
                      , (guideToneLine2 , Just "guide2"    )]
      let outName = (takeBaseName $ optSourceNm os)++"-"++(intercalate "-" . catMaybes $ [mFnNm, tNm])++".ly"
      return $ os { optTargets = catMaybes [t], optOutput = writeFile outName, optMelodyFns = [mFn] }

