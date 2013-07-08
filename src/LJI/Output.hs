{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LJI.Output (

  melodyToLy 

) where

import Prelude hiding (concat, print)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.List hiding (concat)
import Data.Foldable

import LJI.Args (Clef(..))
import LJI.Music (Dur, (%))
import LJI.Music.Notation (Key(..), Tonality(..), Note(..), Pitch(..),
         Class(..), Name(..), Staff(..), Section(..))

-- Top level functions

melodyToLy :: Key -> Clef -> Staff Note -> String
melodyToLy k c nss = execPrinter (PS 0 k c) . scorePrinter $ nss

-- Constants ----

barLength :: Dur
barLength = (1 % 1)

-- Types ----

newtype Printer a =
  Printer { unpackPrinter :: WriterT String (Reader PrinterState) a }
    deriving (Monad, MonadReader PrinterState, MonadWriter String)

data PrinterState = PS { psIndent :: Int
                       , psKey    :: Key
                       , psClef   :: Clef }

execPrinter :: PrinterState -> Printer a -> String
execPrinter s = flip runReader s . execWriterT . unpackPrinter


newtype BarPrinter a =
  BarPrinter { unpackBarPrinter :: StateT BarState (Reader BarEnv) a }
    deriving (Monad, MonadReader BarEnv, MonadState BarState)

type BarState = Dur
type BarEnv   = Dur

runBarPrinter :: BarState -> BarPrinter a -> a
runBarPrinter s = flip runReader s . flip evalStateT s . unpackBarPrinter

-- Utilities ----

infixr 5 *+ 

(*+) :: String -> String -> String
s *+ t = s ++ " " ++ t

-- Indenting printers ----

print :: String -> Printer ()
print m = do l <- psIndent `liftM` ask 
             let i = concat . replicate l $ "  "
             tell $ i ++ m ++ "\n"

indent :: Printer a -> Printer a
indent = local $ \s -> s { psIndent = (+1) . psIndent $ s }

-- Score printers ----

scorePrinter :: Staff Note -> Printer ()
scorePrinter ns = do e <- ask
                     let k = psKey  e
                         c = psClef e
                     print "\\include \"english.ly\""
                     print "\\score {"
                     indent $ do print "\\new Staff {"
                                 indent $ print . clef $ c
                                 indent $ keyPrinter
                                 indent $ staffPrinter ns
                                 print "}"
                     print "}"
  where
   clef FClef = "\\clef bass"
   clef GClef = "\\clef treble"

keyPrinter :: Printer ()
keyPrinter = do e <- ask
                let k = psKey $ e
                    c = clss . kyClass $ k
                    t = tonality . kyTonality $ k
                print $ "\\key" *+ c *+ t

staffPrinter :: Staff Note -> Printer ()
staffPrinter staff = do
  let secs = unwrapStaff $ staff
  print
    . (*+ finalBarLine)
    . intercalate (" " ++ doubleBarLine ++ " ")
    . map section
    $ secs
 where
  doubleBarLine, finalBarLine :: String
  doubleBarLine = "\\bar \"||\" \\break\n"
  finalBarLine  = "\\bar \"|.\" \\break\n"

-- Section printers ----

section :: Section Note -> String
section = runBarPrinter barLength . liftM (intercalate " ") . mapM note . toList

note :: Note -> BarPrinter String
note n = do let c = ptchClass . ntPitch $ n
                o = ptchOctave . ntPitch $ n
                d = ntDur n
            ds <- duration $ d
            return . intercalate " ~ " . map ((clss c ++ octave o) ++) $ ds

-- Pure printers ----

tonality :: Tonality -> String
tonality Major = "\\major"
tonality Minor = "\\minor"

clss :: Class -> String
clss (FlatFlat   n) = name n ++ "ff"
clss (Flat       n) = name n ++ "f"
clss (Natural    n) = name n
clss (Sharp      n) = name n ++ "s"
clss (SharpSharp n) = name n ++ "ss"

name :: Name -> String
name n = case n of
           C -> "c" ; D -> "d" ; E -> "e" ; F -> "f" 
           G -> "g" ; A -> "a" ; B -> "b"

octave :: Int -> String
octave = ([",,,,",",,,",",,",",","","'","''","'''","''''","'''''"] !!) . (+1)

duration :: Dur -> BarPrinter [String]
duration d = do s  <- get
                let d'   = min d s
                    durs = (1%1,"1") : do d <- [2, 4, 8, 16]
                                          m <- [2, 1, 0] 
                                          let f = factor m
                                          return ((1%d)*f, show d ++ dots m) 
                    factor   = ([1%1, 3%2, 7%4] !!)
                    dots     = concat . flip replicate "."
                    (dd,str) = head . dropWhile ((> d') . fst) $ durs
                if s == dd 
                  then do s' <- ask 
                          put s'
                  else put (s - dd)
                if d == dd
                  then return [str]
                  else do strs <- duration (d - dd)
                          return (str : strs)

