module LJI.Parser (

    module Text.Parsec
  , module Text.Parsec.String

  , Chart(..)
  , parseChart
  , wrapParser
  , chartParser
  , rangeParser
  , durParser
  , integerParser
  , listParser

  ) where

import Prelude hiding (mapM, mapM_, foldl, sum)

import Control.Applicative hiding ((<|>), many)
import Control.Monad hiding (mapM, mapM_)
import Control.Newtype

import Data.Char (isSpace)
import Data.Foldable
import Data.Monoid
import Data.Pointed
import qualified Data.Pointed as P (Pointed(..))
import Data.Traversable

import LJI.Music (Relative, Pitch, Class, Dur, Note(..), (%),
         Chord(..))
import LJI.Music.Notation (Key(..), Tonality(..), Name(..),  Staff(..),
         Section(..), Chart(..))
import qualified LJI.Music.Notation as N (Class(..))

import Text.Parsec
import Text.Parsec.String
import Text.Printf

import Debug.Trace

wrapParser :: Monad m => Parser a -> String -> m a
wrapParser p = either (fail . show) return . parse p ""

-- General parsers ----

integerParser = signedParser naturalParser

signedParser :: Num a => Parser a -> Parser a
signedParser num = (*) <$> (option 1 neg) <*> num
 where
  neg = -1 <$ char '-'

naturalParser :: Parser Int
naturalParser =
  do ds <- many digit
     let ds' = map ((+(-48)) . fromEnum) ds
         d   = foldl (\a b -> a*10 + b) 0 ds'
     return d

-- Chart parsers ----

parseChart :: Monad m => String -> m Chart
parseChart = wrapParser (chartParser <* eof) . filter (not . isSpace)

chartParser :: Parser Chart
chartParser =
  do t <- keyParser
     cs <- chords 
     m <- melody <|> return mempty
     return $ Chart t cs m


chords :: Parser (Staff Chord)
chords = (pack . fromList) <$> bars <* string "|||"
 where
  bars :: Parser [Section Chord]
  bars = sectionParser chordsParser 4
           `sepBy` try (string "||" >> notFollowedBy (char '|'))

melody :: Parser (Staff (Class, Dur))
melody = (pack . fromList) <$> bars <* string "|||"
 where
  bars :: Parser [Section (Class, Dur)]
  bars = sectionParser (const melodyParser) 4
           `sepBy` try (string "||" >> notFollowedBy (char '|'))

-- Key parser ----

keyParser :: Parser Key
keyParser = do c <- classParser
               t <- char 'm' *> return Minor
                     <|> return Major
               return $ Key c t

classParser :: Parser N.Class
classParser = do n <- keyNameParser
                 (flatParser n
                   <|> sharpParser n
                   <|> return (N.Natural n))
 where
  flatParser :: Name -> Parser N.Class
  flatParser n = do char 'b'
                    char 'b' *> return (N.FlatFlat n)
                      <|> return (N.Flat n)
  sharpParser :: Name -> Parser N.Class
  sharpParser n = do char '#'
                     (char '#' *> return (N.SharpSharp n)
                       <|> return (N.Sharp n))

keyNameParser :: Parser Name
keyNameParser = do n <- oneOf "ABCDEFGabcdefg"
                   return $ case n of
                             'A' -> A ; 'a' -> A ; 'B' -> B ; 'b' -> B
                             'C' -> C ; 'c' -> C ; 'D' -> D ; 'd' -> D
                             'E' -> E ; 'e' -> E ; 'F' -> F ; 'f' -> F
                             'G' -> G ; 'g' -> G

-- Section parsers (Anything between bars and ended by double bar lines) ----

sectionParser :: (Int -> Parser (Section a)) -> Int -> Parser (Section a)
sectionParser p n =
  mconcat <$> p n `sepBy` try (char '|' >> notFollowedBy (char '|'))

-- chordsParser - a parser parameterised on the number of chrochets in a bar
--  that parses a succession of chords annotated for rhythm with slashes

chordsParser :: Int -> Parser (Section Chord)
chordsParser n = do
  cs <- many1 chordParser
  when (dur cs > toInteger n % 4)
       (fail . printf "Bar exceeds %d beats" $ n)
  when (dur cs < toInteger n % 4)
       (fail . printf "Bar has fewer than %d beats" $ n)
  return (fromList cs)
 where
  dur = sum . map chdDur

-- Chord parser ----

chordParser :: Parser Chord
chordParser = do
  nm  <- noteNameParser
  a   <- option 0 accidentalParser
  let r = fromEnum nm + a
  tns <- fmap (toEnum . (+ r))
          <$> option [4, 7, 12] chordTypeParser
  d   <- length <$> many (char '/')
  return $ Chord (toEnum r) (tns !! 0) (tns !! 1) (tns !! 2) (countBeats d) 
 where 
  countBeats = (%4) . (+1) . toInteger 

chordTypeParser :: Parser [Relative]
chordTypeParser =      [4, 7, 11] <$ char '^'
                   <|> [4, 7, 10] <$ char '7'
                   <|> [4, 7,  9] <$ char '6'
                   <|> [3, 7, 10] <$ (try . string $ "m7")
                   <|> [3, 7, 11] <$ (try . string $ "m^7")
                   <|> [3, 7,  9] <$ (try . string $ "m6")
                   <|> [3, 7, 12] <$ char 'm'
                   <|> [3, 6, 10] <$ char '0'
                   <|> [3, 6,  9] <$ (try . string $ "o7")
                   <|> [3, 6, 12] <$ char 'o'
                   <|> [3, 8, 10] <$ (try . string $ "+7")
                   <|> [3, 8, 12] <$ char '+'

-- Range parser ----

rangeParser :: Parser (Pitch, Pitch)
rangeParser = do liftM2 (,) (pitchParser <* char '~') pitchParser

-- Dur parser ----

durParser :: Parser Dur
durParser = do n <- toInteger `liftM` naturalParser
               s <- option (1%1) (do char '.'
                                     option (3%2) (do char '.'
                                                      return (9%4)
                                                  )
                                 )
               return $ (1%n) * s

-- Pitch parser ----

pitchParser :: Parser Pitch
pitchParser = do n <- noteNameParser
                 a <- option 0 accidentalParser
                 o <- octaveParser
                 return $ fromEnum n + a + ((o+1) * 12)

noteNameParser :: Parser Class
noteNameParser = do n <- oneOf "CDEFGABcdefgab"
                    return . toEnum $ case n of
                      'C' -> 0  ; 'c' -> 0  ; 'D' -> 2  ; 'd' -> 2
                      'E' -> 4  ; 'e' -> 4  ; 'F' -> 5  ; 'f' -> 5
                      'G' -> 7  ; 'g' -> 7  ; 'A' -> 9  ; 'a' -> 9
                      'B' -> 11 ; 'b' -> 11

accidentalParser :: Parser Int
accidentalParser = (negate . length) <$> many1 (char 'b')
                     <|> length <$> many1 (char '#') 

octaveParser :: Parser Int
octaveParser = do o <- integerParser
                  if (-2) < o && o < 10
                   then return o
                   else fail "Octave not in range -1 to 9"

-- List parsers ----

listParser :: Parser a -> Parser [a]
listParser elem = between (char '[') (char ']') elems
  where elems = (spaces *> elem <* spaces) `sepBy` char ','

-- Melody parser ----

melodyParser :: Parser (Section (Class, Dur))
melodyParser = fromList <$> many melNoteParser

melNoteParser :: Parser (Class, Dur)
melNoteParser = (,) <$> (augment <$> noteNameParser
                                 <*> (accidentalParser <|> return 0))
                    <*> durParser
 where
  augment c a = toEnum . (+a) . fromEnum $ c
