module LJI.Console (
    promptYesNo
  , promptConfirmation
  , promptConfirmation'
  , putStringAndExit
  , putErrorAndExit
  ) where

import System.Exit (ExitCode(..), exitWith)

import LJI.Name

promptYesNo :: String -> IO a -> IO a -> IO a
promptYesNo s y n = do putStrLn s
                       r <- getContents
                       case r of
                         ('y':_) -> y ; ('Y':_) -> y
                         ('n':_) -> n ; ('N':_) -> n
                         otherwise -> promptYesNo s y n

promptConfirmation' :: String -> IO ()
promptConfirmation' s = promptConfirmation s (return ())

promptConfirmation :: String -> IO () -> IO ()
promptConfirmation s n = promptYesNo s (return ()) (n >> exitWith ExitSuccess)

putErrorAndExit :: String -> IO a
putErrorAndExit err = do putStrLn (programName ++ ": " ++ err)
                         exitWith (ExitFailure 1)

putStringAndExit :: String -> IO a
putStringAndExit mssg = do putStrLn mssg
                           exitWith ExitSuccess
