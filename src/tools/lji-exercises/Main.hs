{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, DeriveFunctor,
             GeneralizedNewtypeDeriving #-}
module Main (
    main
  ) where

import System.Environment (getArgs)
import System.Exit
import System.Process (system)
import System.IO

import Text.Printf

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans

import Args

-- Shell scripting definitions
-------------------------------------------------------------------------------

--
-- Provides a Shell and Priv monad, for encapulating errors in
-- shell programs nicely, and for static separation of code requiring root
-- privledges from other code.

--
-- The 'Shell' monad, a wrapper over IO that captures failure in an
-- error transformer.
--

newtype Shell a = Shell { runShell :: ErrorT String IO a }
  deriving (Functor, Monad, MonadIO)

--
-- The 'Priv' monad, a shell monad for commands requiring root
-- privledges. Let's us distinguish such command statically, on the type
-- level.
--
-- To run something in the Priv monad, use 'priv'.
--
newtype Priv a = Priv { priv :: Shell a }
  deriving (Functor, Monad, MonadIO)

--
-- Rather than just derive error handling, we'll roll our own that
-- propagates shell failures into errors.
--
instance MonadError String Shell where
  throwError = error . ("Shell failed: "++)
instance MonadError String Priv  where
  throwError = error . ("Priv failed: "++)

-- Run a normal shell command as the user. Return either a result or an error value
shell :: Shell a -> IO (Either String a)
shell = runErrorT . runShell

-- Run a privileged command, requiring sudo access. Return any output
runPriv :: String -> Priv ()
runPriv = Priv . run . ("/usr/bin/sudo " ++)

--
-- Convenient wrapper
--
io :: IO a -> Shell a
io = liftIO

--
-- Run a shell command, wrapping any errors in ErrorT
--
run :: String -> Shell ()
run s = do io . putStrLn $ s
           io . system $ s
           return ()
                

-- Script
-------------------------------------------------------------------------------

type Melody = String
type Position = String

nameGroups = [ ("1a", "[-1]"        )
             , ("1b", "[1]"         )
             , ("2a", "[-1,1]"      )
             , ("2b", "[1,-1]"      )
             , ("3a", "[-2,-1,1]"   )
             , ("3b", "[2,1,-1]"    )
             , ("4a", "[-2,-1,2,1]" )
             , ("4b", "[2,1,-2,-1]" )
             , ("5a", "[-3,-2,-1,1]")
             , ("5b", "[3,2,1,-1]"  )
             ]

mels = [ "root"
       , "guide1"
       , "guide2"
       ]

positions = [ "E1~C3"
            , "F1~C#3"
            , "F#1~D3"
            , "G1~D#3"
            , "G#1~E3"
            , "A1~F3"
            , "A#1~F#3"
            , "B1~G3"
            , "C2~G#3"
            , "C#2~A3"
            , "D2~A#3"
            , "D#2~B3"
            ]

main :: IO ()
main = do args <- getArgs
          opts <- readOpts args
          either putStrLn (\_ -> return ()) =<< (shell . script $ opts)

script :: Options -> Shell ()
script op = do mapM_ (exercises op) mels
               run "echo \"Cleaning up\""
               run "rm *.ps"
 
exercises :: Options -> Melody -> Shell ()
exercises ops l = do sequence_ . regularExs ops $ l
                     sequence_ . targetedExs ops $ l

targetedExs :: Options -> Melody -> [Shell ()]
targetedExs ops l =
  do let range    = optRange ops
         clef     = optClef  ops
         dur      = optDur ops
         flags    = "-c" ++ clef ++ " -r" ++ range ++ " -d" ++ dur
         inFile   = optInput ops
     (gn, g) <- nameGroups
     let outFile = takeWhile (/= '.') inFile
                    ++ "-" ++ l ++ "-" ++ gn ++ ".ly"
     return (do run$ "LJI " ++ l ++ " " ++ flags ++ " -t" ++ g ++ " " ++ inFile
                       ++ " > " ++ outFile
                run$ "lilypond --pdf " ++ outFile)

regularExs :: Options -> Melody -> [Shell ()]
regularExs ops l =
  do let range   = optRange ops
         clef    = optClef  ops
         flags   = "-c" ++ clef ++ " -r" ++ range
         inFile  = optInput ops
         outFile = takeWhile (/= '.') inFile ++ "-" ++ l ++ ".ly"
     return (do run $ "LJI " ++ l ++ " " ++ flags ++ " " ++ inFile
                        ++ " > " ++ outFile
                run $ "lilypond --pdf " ++ outFile)
