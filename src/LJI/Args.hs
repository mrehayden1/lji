{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LJI.Args (
    Options(..)
  , Range
  , Clef(..)
  , parseArgs
  ) where

import System.Directory
import System.Exit

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Error ()
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Data.Char
import Data.Traversable

import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Error

import LJI.Console (promptConfirmation, putErrorAndExit)
import LJI.Music (Class, Pitch, Dur, Chord, (%))
import LJI.Music.Notation (Staff, Section, Chart(..))
import LJI.Music.Composer (rootProgression, fifths, rootsFifths1, rootsFifths2,
         guideToneLine1, guideToneLine2)
import LJI.Name (programTitle, version)
import LJI.Parser (rangeParser, durParser, listParser, integerParser)

-- Types

type Range = (Pitch, Pitch)

data Options = Options {
    optVerbose   :: Bool
  , optRange     :: Range
  , optClef      :: Clef
  , optTargets   :: [[Int]]
  , optDur       :: Dur
  , optMix       :: Maybe Dur 
  , optMelodyFns :: [Chart -> Staff (Class, Dur)]
  , optSource    :: IO String
  , optSourceNm  :: String
  , optOutput    :: String -> IO ()
  , optArgString :: String
  }

instance Show Options where
  show os =
       "{"
    ++ " optVerbose = " ++ show (optVerbose os) ++ ","
    ++ " optRange = " ++ show (optRange os) ++ ","
    ++ " optClef = " ++ show (optClef os) ++ ","
    ++ " optTargets = " ++ show (optTargets os) ++ ","
    ++ " optDur = " ++ show (optDur os)
    ++ " optMix = " ++ show (optMix os)
    ++ " }"

defaultOptions :: Options
defaultOptions = Options {
                   optVerbose   = False
                 , optRange     = (60, 71) -- C4 - B4
                 , optClef      = GClef
                 , optTargets   = []
                 , optDur       = 1%8
                 , optMix       = Nothing 
                 , optMelodyFns = []
                 , optSource    = getContents
                 , optSourceNm  = "standard input"
                 , optOutput    = putStr
                 , optArgString = ""
                 }

---- Useful functions

wrapParser :: Monad m => Parser a -> String -> m a
wrapParser p = either (fail . formatErrors) return . parse p ""
 where
  formatErrors = showErrorMessages "or" "unknown parse error" "expecting"
                                       "unexpected" "end of input"
                   . errorMessages

---- Useful parsers

spaces1 :: Parser ()
spaces1 = skipMany1 space

---- Top level parser

parseArgs :: Monad m => String -> m (Either String Options)
parseArgs args = do
  opts <- wrapParser (spaces *> argsParser <* spaces <* eof) args
  case opts of
    Right os -> return $ Right $ os { optArgString = args }
    l        -> return l

argsParser :: Parser (Either String Options)
argsParser = do
  opts <- optsParser
  spaces
  source <- return `option` parseSource
  spaces
  output <- return `option` parseOutput
  return $ foldl (>>=) (return defaultOptions) [opts, source, output]

parseSource, parseOutput :: Parser (Options -> Either String Options)
parseSource = setSource <$> many1 (satisfy (not . isSpace))
 where
  setSource :: String -> Options -> Either String Options
  setSource file os = return $ os { optSource = readFile file, optSourceNm = file }

parseOutput = setOutput <$> many1 (satisfy (not . isSpace))
 where 
  setOutput :: String -> Options -> Either String Options
  setOutput file os = return $ os { optOutput = overwriteFile file }

overwriteFile :: String -> String -> IO ()
overwriteFile file out = do
  e <- doesFileExist file
  when e (promptFileExists file)
  writeFile file out

promptFileExists :: String -> IO () 
promptFileExists p = promptConfirmation ("LJI: \"" ++ p
                                           ++ "\" file already exists. "
                                           ++ "Overwrite? (y/n): ")
                                        (putStrLn "Exiting...")

---- Options and switches passed to the program

optsParser :: Parser (Options -> Either String Options)
optsParser = do
  opts <- liftM2 (++) operations switches
  return $ \os -> foldl (>>=) (return os) opts

operations, switches :: Parser [Options -> Either String Options]
operations =     (:) <$> try operation <*> (spaces *> operations)
             <|> return []
switches   =     (:) <$> try switch <*> (spaces *> switches)
             <|> return []

---- Operation arguments, the first argument received, if at all

data Clef = GClef | FClef
 deriving (Show)

type OptionsParser = Parser (Options -> Either String Options)

operation :: OptionsParser
operation = do
  opr <- optParser
  return $ \os -> return $ os { optMelodyFns = optMelodyFns os ++ [opr] }
 where
  optParser :: OperationParser
  optParser =
       (try melodyCmd <?> "")
   <|> (try rootProgressionCmd <?> "")
   <|> (try fifthsCmd <?> "")
   <|> (try rootsFifths1Cmd <?> "")
   <|> (try rootsFifths2Cmd <?> "")
   <|> (try guideToneLine1Cmd <?> "")
   <|> (try guideToneLine2Cmd <?> "")
   <|> unknownCommand
   <?> "operation"

type OperationParser = Parser (Chart -> Staff (Class, Dur))

melodyCmd :: OperationParser
melodyCmd = chrtMelody <$ (string "melody" *> notFollowedBy alphaNum)

rootProgressionCmd :: OperationParser
rootProgressionCmd =
  rootProgression <$ (string "root" *> notFollowedBy alphaNum)

fifthsCmd :: OperationParser
fifthsCmd =
  fifths <$ (string "fifth" *> notFollowedBy alphaNum)

rootsFifths1Cmd :: OperationParser
rootsFifths1Cmd =
  rootsFifths1 <$ (string "rootfifth1" *> notFollowedBy alphaNum)

rootsFifths2Cmd :: OperationParser
rootsFifths2Cmd =
  rootsFifths2 <$ (string "rootfifth2" *> notFollowedBy alphaNum)

guideToneLine1Cmd :: OperationParser
guideToneLine1Cmd =
  guideToneLine1 <$ (string "guide1" *> notFollowedBy alphaNum)

guideToneLine2Cmd :: OperationParser
guideToneLine2Cmd =
  guideToneLine2 <$ (string "guide2" *> notFollowedBy alphaNum)

unknownCommand :: Parser a
unknownCommand = do
  c <- many1 (letter <?> "")
  unexpected ("command " ++ c)

---- Switches

switch :: OptionsParser
switch = do {
    arg <- try validSwitch -- Switch parsers return their argument parser
  ; arg
  } <|> unknownSwitch

validSwitch :: Parser OptionsParser 
validSwitch = do
        helpSwitch
    <|> versionSwitch
    <|> verboseSwitch
    <|> rangeSwitch
    <|> durSwitch
    <|> targetSwitch
    <|> mixSwitch
    <|> clefSwitch

unknownSwitch :: Parser a
unknownSwitch = do
  s <-  try ((++) <$> string "-" <*> count 1 letter)
    <|> ((++) <$> string "--" <*> many1 (letter <?> ""))
  unexpected s

helpSwitch :: Parser OptionsParser
helpSwitch = do
  try (string "--help")
    <|> try (string "-h")
  return (pure failWithUsage)
 where
  failWithUsage :: Options -> Either String Options
  failWithUsage = const (Left usage)

versionSwitch :: Parser OptionsParser
versionSwitch = do
  try (string "--version")
    <|> try (string "-V")
  return (pure failWithVersion)
 where
  failWithVersion :: Options -> Either String Options
  failWithVersion = const $ (Left $ programTitle ++ " v" ++ version)

verboseSwitch :: Parser OptionsParser
verboseSwitch = do
  try (string "--verbose")
    <|> try (string "-v")
  return (pure setVerbose)
 where
  setVerbose :: Options -> Either String Options
  setVerbose o = Right (o { optVerbose = True })

mixSwitch :: Parser OptionsParser
mixSwitch = do
  try (string "--mix")
    <|> try (string "-m")
  spaces
  return (setMix <$> durParser)
 where
   setMix :: Dur -> Options -> Either String Options
   setMix d o = Right (o { optMix = Just d })

rangeSwitch :: Parser OptionsParser
rangeSwitch = do
  try (string "--range")
    <|> try (string "-r")
  spaces
  return (setValidRange <$> rangeParser)
 where
  setValidRange :: Range -> Options -> Either String Options
  setValidRange r@(a, b)
    | (a < b)   = \o -> Right $ o { optRange = r }
    | otherwise = const . Left $ "first value in a range should be"
                    ++ " less than the second"

durSwitch :: Parser OptionsParser
durSwitch = do
  try (string "--duration")
    <|> try (string "-d")
  spaces
  return (setDur <$> durParser)
 where
  setDur :: Dur -> Options -> Either String Options
  setDur d o = Right $ o { optDur = d }

targetSwitch :: Parser OptionsParser
targetSwitch = do
  try (string "--target")
    <|> try (string "-t")
  spaces
  return (addTarget <$> listParser integerParser)
 where
  addTarget :: [Int] -> Options -> Either String Options
  addTarget t o = Right $ o { optTargets = optTargets o ++ [t] }

clefSwitch :: Parser OptionsParser
clefSwitch = do
  try (string "--clef")
    <|> try (string "-c")
  spaces
  return (setClef <$> clefArg)
 where
  clefArg :: Parser Clef
  clefArg = oneOf "fF" *> return FClef <|> oneOf "gG" *> return GClef
             <?> "one of either 'F', 'f', 'G' or 'g'"
  setClef :: Clef -> Options -> Either String Options
  setClef c o = Right $ o { optClef = c }

---- Usage notes

usage :: String
usage =     "Usage: LJI --help\n"
         ++ "       LJI --version\n"
         ++ "       LJI [COMMAND...] [OPTS...] [INPUT] [OUTPUT]\n"
         ++ "\n"
         ++ "Commands include:\n"
         ++ "  melody  Use the melody\n"
         ++ "  root    Derive the root progression\n"
         ++ "  fifth   Derive the fifths of a progression\n"
         ++ "  guide1  Derive the 1st guide-tone line\n"
         ++ "  guide2  \"\"     \"\"  2nd \"\"    \"\"   \"\"\n"
         ++ "\n"
         ++ argUsage ++ "\n"
         ++ "\n"
         ++ notes
         ++ "\n"
         ++ optUsage

argUsage :: String
argUsage =     "Arguments:\n"
            ++ "  INPUT   The input source, the default is the standard input\n"
            ++ "  OUTPUT  The output location, the default is the standard output"

notes :: String
notes =     "Notes:\n"
         ++ "  If OUTPUT already exists the target file is overwritten. "
         ++ "If OUTPUT is missing, output is sent to the standard output"

optUsage :: String
optUsage =    "Options:\n"
           ++ "  --verbose  -v  Increase program output\n"
           ++ "  --mix      -m  Mix lines instead of concatenating them. (Unimplemented)\n"
           ++ "  --range    -r  Set the range from which notes should be chosen. Specify two pitches separated by a ~, e.g. A1~B3\n"
           ++ "  --duration -d  The length of the individual notes that form the targeting of a melody. See --target for more info.\n"
           ++ "  --target   -t  Target the derived melody with the specified sequence of chromatic differences. Specify a list of pitch changes e.g. [1,-2,1]. Sequences that are too long will be truncated.\n"
           ++ "  --clef     -c  Change output notation clef. Specify either 'G' or 'F' for either clef.\n"

