module LJI.Music.Notation.Scales (
    Direction(..),
    chromaticScale
  ) where

import Control.Monad.State (State(..))

import LJI.Music (Note(..))
import LJI.Music.Notation (Key, Tonality(..), Name(..), Class(..), Octave)
import LJI.Music.Notation

data Direction = Asc | Desc
  deriving (Eq)

cyc :: Int -> [a] -> [a]
cyc n = take 12 . drop n . cycle

chromaticScale :: Monad m => Key -> Direction -> m [Class]
chromaticScale (Key c t) = chromaticScale' c t

chromaticScale' :: Monad m => Class -> Tonality -> Direction -> m [Class]
-- C Major  1st       2nd       3rd  4th       5th       6th       7th
chromaticScale' (Natural C) Major Asc  = return . cyc  0 $
           [c  , cs , d  , ds , e  , f  , fs , g  , gs , a  , as , b  ]

chromaticScale' (Natural C) Major Desc = return . cyc  0 $
           [c  , df , d  , ef , e  , f  , gf , g  , af , a  , bf , b  ]

-- F Major  1st       2nd       3rd  4th       5th       6th       7th
chromaticScale' (Natural F) Major Asc  = return . cyc  7 $ 
           [f  , fs , g  , gs , a  , bf , b  , c  , cs , d  , ds , e  ]

chromaticScale' (Natural F) Major Desc = return . cyc  7 $
           [f  , gf , g  , af , a  , bf , cf , c  , df , d  , ef , e  ]

-- Bb Major 1st       2nd       3rd  4th       5th       6th       7th
chromaticScale' (Flat B)    Major Asc  = return . cyc  2 $
           [bf , b  , c  , cs , d  , ef , e  , f  , fs , g  , gs , a  ]

chromaticScale' (Flat B)    Major Desc = return . cyc  2 $
           [bf , cf , c  , df , d  , ef , ff , f  , gf , g  , af , a  ]
 
-- Eb Major 1st       2nd       3rd  4th       5th       6th       7th
chromaticScale' (Flat E)    Major Asc  = return . cyc  9 $
           [ef , e  , f  , fs , g  , af , a  , bf , b  , c  , cs , d  ]

chromaticScale' (Flat E)    Major Desc = return . cyc  9 $
           [ef , ff , f  , gf , g  , af , bff, bf , cf , c  , df , d  ]

-- Ab Major 1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Flat A)    Major Asc  = return . cyc  4 $
           [af , a  , bf , b  , c  , df , d  , ef , e  , f  , fs , g  ]

chromaticScale' (Flat A)    Major Desc = return . cyc  4 $
           [af , bff, bf , cf , c  , df , eff, ef , ff , f  , gf , g  ]

-- Db Major 1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Flat D)    Major Asc  = return . cyc 11 $
           [df , d  , ef , e  , f  , gf , g  , af , a  , bf , b  , c  ]

chromaticScale' (Flat D)    Major Desc = return . cyc 11 $
           [df , eff, ef , ff , f  , gf , aff, af , bff, bf , cf , c  ]

-- Gb Major 1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Flat G)    Major Asc  = return . cyc  6 $
           [gf , g  , af , a  , bf , cf , c  , df , d  , ef , e  , f  ]

chromaticScale' (Flat G)    Major Desc = return . cyc  6 $
           [gf , aff, af , bff, bf , cf , dff, df , eff, ef , ff , f  ] 
-- F# Major 1st       2nd       3rd  4th       5th       6th       7th
chromaticScale' (Sharp F)   Major Asc  = return . cyc  6 $
           [fs , fss, gs , gss, as , b  , bs , cs , css, ds , dss, es ]

chromaticScale' (Sharp F)   Major Desc = return . cyc  6 $
           [fs , g  , gs , a  , as , b  , c  , cs , d  , ds , e  , es ]

-- B Major  1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Natural B) Major Asc  = return . cyc  1 $
           [b  , bs , cs , css, ds , e  , es , fs , fss, gs , gss, as ]

chromaticScale' (Natural B) Major Desc = return . cyc  1 $ [b  , c  , cs , d  , ds , e  , f  , fs , g  , gs , a  , as ]

-- E Major  1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Natural E) Major Asc  = return . cyc  8 $
           [e  , es , fs , fss, gs , a  , as , b  , bs , cs , css, ds ]

chromaticScale' (Natural E) Major Desc = return . cyc  8 $
           [e  , f  , fs , g  , gs , a  , bf , b  , c  , cs , d  , ds ]

-- A Major  1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Natural A) Major Asc  = return . cyc  3 $
           [a  , as , b  , bs , cs , d  , ds , e  , es , fs , fss, gs ]

chromaticScale' (Natural A) Major Desc = return . cyc  3 $
           [a  , bf , b  , c  , cs , d  , ef , e  , f  , fs , g  , gs ]

-- D Major  1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Natural D) Major Asc  = return . cyc 10 $
           [d  , ds , e  , es , fs , g  , gs , a  , as , b  , bs , cs ]

chromaticScale' (Natural D) Major Desc = return . cyc 10 $
           [d  , ef , e  , f  , fs , g  , af , a  , bf , b  , c  , cs ]

-- G Major  1st       2nd       3rd  4th       5th       6th       7th 
chromaticScale' (Natural G) Major Asc  = return . cyc  5 $
           [g  , gs , a  , as , b  , c  , cs , d  , ds , e  , es , fs ]

chromaticScale' (Natural G) Major Desc = return . cyc  5 $
           [g  , af , a  , bf , b  , c  , df , d  , ef , e  , f  , fs ]

-- Relative Minors
chromaticScale' (Natural A) Minor dir  = chromaticScale' c  Major dir
chromaticScale' (Natural D) Minor dir  = chromaticScale' f  Major dir
chromaticScale' (Natural G) Minor dir  = chromaticScale' bf Major dir
chromaticScale' (Natural C) Minor dir  = chromaticScale' ef Major dir
chromaticScale' (Natural F) Minor dir  = chromaticScale' af Major dir
chromaticScale' (Flat B)    Minor dir  = chromaticScale' df Major dir
chromaticScale' (Flat E)    Minor dir  = chromaticScale' gf Major dir
chromaticScale' (Sharp D)   Minor dir  = chromaticScale' fs Major dir
chromaticScale' (Sharp G)   Minor dir  = chromaticScale' b  Major dir
chromaticScale' (Sharp C)   Minor dir  = chromaticScale' e  Major dir
chromaticScale' (Sharp F)   Minor dir  = chromaticScale' a  Major dir
chromaticScale' (Natural B) Minor dir  = chromaticScale' d  Major dir
chromaticScale' (Natural E) Minor dir  = chromaticScale' g  Major dir

-- And the rest that (mostly) don't exist in western notation
chromaticScale' _           _     _    = fail ("Key does not exist." ++ 
                                           " Use an enharmonic equiv." ++
                                           " from the cycle of fourths.")
