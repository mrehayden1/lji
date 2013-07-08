{-# LANGUAGE FlexibleInstances #-}
module LJI.Music (

    module Data.Ratio

  , Tie(..)
  , Enharm(..)

    -- * Musical primitives and first order constructs
    -- | Basic musical building blocks
  , Pitch
  , Relative
  , Dur
  , Class
  , Note(..)
  , Chord(..)
  , classDist
  , minPitch
  , maxPitch
  , pitchClass

  ) where

import Data.Ratio (Rational, (%))

class Tie a where
  tie :: a -> a -> a

class Enharm a where
  enharm :: a -> a -> Bool

-- Pitches are represented as a series of integers, one for each discrete
-- pitch beginning at C-1.
type Pitch = Int

minPitch, maxPitch :: Int
minPitch = 0
maxPitch = 131

type Relative = Int

-- A pitch name or 'class' can be seen as a the distance a pitch is from a
-- reference note. For the sake of simplicity this is always assumed to be the
-- C below the note.
newtype Class = Class Int
  deriving (Eq, Ord)

instance Show Class where
  show (Class c) = case c of
    0 -> "C"     ; 1  -> "C#/Db" ; 2  -> "D"     ; 3  -> "D#/Eb"
    4 -> "E"     ; 5  -> "F"     ; 6  -> "F#/Gb" ; 7  -> "G"
    8 -> "G#/Ab" ; 9  -> "A"     ; 10 -> "A#/Bb" ; 11 -> "B"
    _ -> error "show : Class does not exist"

instance Enum Class where
  fromEnum (Class c) = c
  toEnum   n         = Class $ n `mod` 12

pitchClass :: Pitch -> Class
pitchClass = toEnum

-- classDist - the minimum number of semi-tones (up/down) between two named
-- pitches
classDist :: Class -> Class -> Int
classDist (Class c) (Class d) | c < d     = classDist' c d
                              | otherwise = classDist' d c
  where
    classDist' c d = min (abs (c - d)) (abs (c + 12 - d))

type Dur = Rational

-- | Type wrapper for a representation of a pitch sounded for a specified duration
data Note = Note { ntPitch :: Pitch
                 , ntDur   :: Dur
                 }
  deriving (Show)

instance Enharm Note where
  Note p _ `enharm` Note p' _ = p == p'

instance Tie Note where
  Note p d `tie`  Note _ d' = Note p (d+d')

instance Eq a => Enharm (a, Dur) where
  (a, _) `enharm` (b, _) = a == b

instance Tie (a, Dur) where
  (a, d) `tie`  (_, d') = (a, d+d')

data Chord = Chord { chdRoot    :: Class
                   , chdThird   :: Class
                   , chdFifth   :: Class
                   , chdSeventh :: Class
                   , chdDur     :: Dur
                   }
  deriving (Show, Eq)

instance Tie Chord where
  Chord r t f s d `tie` Chord _ _ _ _ d' = Chord r t f s (d + d')

