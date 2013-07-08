module Args (
   Options(..)
 , Range
 , defaultOptions
 , helpOptDescr
 , rangeOptDescr
 , durOptDescr
 , clefOptDescr
 , optDescrs
 , readOpts
 , printUsage
 ) where

import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..),
                              usageInfo, getOpt)
import System.Exit

import Data.List (intercalate)

import Main.Args (Range, Clef(..), readRange, readDur)


type Filename = String

data Options = Options { optInput :: Filename
                       , optRange :: String
                       , optClef  :: String
                       , optDur   :: String
                       }

defaultOptions :: IO Options
defaultOptions = return $ Options { optInput = error "No input specified"
                                  , optRange = "C4~B4"
                                  , optClef  = "g"
                                  , optDur   = "8"
                                  }
helpOptDescr :: OptDescr (Options -> IO Options)
helpOptDescr =
  Option ""
         ["help"]
         (NoArg (\_ -> do printUsage
                          exitWith ExitSuccess
                )
         )
         "Display help"

rangeOptDescr :: OptDescr (Options -> IO Options)
rangeOptDescr =
  Option "r" 
         ["range"]
         (ReqArg rangeOpt "RNG")
         "Set the range in which compositions are generated (Default (C4, B4))"
 where
  rangeOpt s o = either error
                        (\_ -> return (o { optRange = s }))
                  . readRange $ s

durOptDescr :: OptDescr (Options -> IO Options)
durOptDescr =
  Option "d"
         ["duration"]
         (ReqArg durOpt "DUR")
         "Set the duration of the embellishment notes applied to the melodies"
 where
  durOpt s o = either error
                      (\_ -> return (o { optDur = s }))
                . readDur $ s


clefOptDescr :: OptDescr (Options -> IO Options)
clefOptDescr =
  Option "c"
         ["clef"]
         (ReqArg clefOpt "[f|g]")
         "Generate music in the clef supplied in the argument (Default : G Clef)"
  where
    clefOpt :: String -> Options -> IO Options
    clefOpt ('f':_) os = return $ os { optClef = "f" }
    clefOpt ('g':_) os = return $ os { optClef = "g" }

optDescrs = [ helpOptDescr
            , rangeOptDescr
            , durOptDescr
            , clefOptDescr
            ]

readOpts :: [String] -> IO Options
readOpts args =
  case getOpt RequireOrder optDescrs args of
    (optss, []   , [] ) -> do opts <- foldl (>>=)
                                            defaultOptions
                                            optss
                              error "There was no input file given"
    (optss, (n:_), [] ) -> do opts <- foldl (>>=)
                                             defaultOptions
                                             optss
                              return $ opts { optInput = n }
                               
    (_    , _    , ers) -> error $ "There were errors reading the arguments."
                            ++ intercalate " " ers


printUsage :: IO ()
printUsage =
  do putStrLn . usageInfo
      ("Usage: LJIexercisese -" ++ optDescrShorts optDescrs ++ " INPUT\n"
       ++ "  LJI batch creation of exercises, output is created in files named"
       ++ " after the input file with a suffix corresponding to the exercise"
       ++ " type.")
      $ optDescrs
 where
   optDescrShorts = concatMap optDescrShort
   optDescrShort (Option s _ _ _) = s
