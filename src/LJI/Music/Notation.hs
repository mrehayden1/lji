{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LJI.Music.Notation (

    Name(..)
  , Class(..)
  , Octave
  , Pitch(..)
  , Note(..)
  , cff, dff, eff, fff, gff, aff, bff, cf, df, ef, ff, gf, af, bf, c, d, e, f
  , g, a, b, cs, ds, es, fs, gs, as, bs, css, dss, ess, fss, gss, ass, bss
  , Tonality(..)
  , Key(..)

    -- * Data structures representing musical forms
  , Staff(..)
  , Section(..)

  , Chart(..)

  ) where

import Prelude hiding (mapM, foldr, foldl)

import Control.Applicative
import Control.Newtype

import Data.Foldable
import Data.Monoid
import Data.Traversable

import qualified Data.Pointed as P (Pointed(..))
import Data.Pointed

import qualified LJI.Music as M (Pitch, Relative, Dur(..), Chord(..), Class)

data Name = C | D | E | F | G | A | B
  deriving (Show, Eq)
data Class = Natural Name
           | Flat Name
           | Sharp Name
           | FlatFlat Name
           | SharpSharp Name
  deriving (Show, Eq)

type Octave = Int
data Pitch = Pitch { ptchClass  :: Class,
                     ptchOctave :: Octave }
  deriving (Eq, Show)

data Note = Note { ntPitch :: Pitch,
                   ntDur   :: M.Dur }
  deriving (Eq, Show)

cff, dff, eff, fff, gff, aff, bff, cf, df, ef, ff, gf, af, bf, c, d, e :: Class
f, g, a, b, cs, ds, es, fs, gs, as, bs, css, dss, ess, fss, gss, ass :: Class
bss :: Class
cff = FlatFlat C
dff = FlatFlat D
eff = FlatFlat E
fff = FlatFlat F
gff = FlatFlat G
aff = FlatFlat A
bff = FlatFlat B
cf  = Flat C
df  = Flat D
ef  = Flat E
ff  = Flat F
gf  = Flat G
af  = Flat A
bf  = Flat B
c   = Natural C
d   = Natural D
e   = Natural E
f   = Natural F
g   = Natural G
a   = Natural A
b   = Natural B
cs  = Sharp C
ds  = Sharp D
es  = Sharp E
fs  = Sharp F
gs  = Sharp G
as  = Sharp A
bs  = Sharp B
css = SharpSharp C
dss = SharpSharp D
ess = SharpSharp E
fss = SharpSharp F
gss = SharpSharp G
ass = SharpSharp A
bss = SharpSharp B


data Tonality = Major | Minor
  deriving (Show, Eq)


data Key = Key { kyClass    :: Class
               , kyTonality :: Tonality }
 deriving (Show, Eq)

  
newtype Section a = WrapSection { unwrapSection :: [a] }
  deriving (Show, Eq, Functor, Pointed, Applicative, Monoid, Foldable,
            Traversable, Monad)

instance Newtype (Section a) ([a]) where
  pack   = WrapSection
  unpack = unwrapSection


newtype Staff a = WrapStaff { unwrapStaff :: [Section a] }
 deriving (Show, Eq)

instance Newtype (Staff a) ([Section a]) where
  pack   = WrapStaff
  unpack = unwrapStaff

instance Functor Staff where
  fmap = over WrapStaff . fmap . fmap

instance Pointed Staff where
  pure = pack . P.pure . P.pure

instance Monoid (Staff a) where
  mempty        = pack mempty
  a `mappend` b = pack $ unpack a `mappend` unpack b

instance Foldable Staff where
  foldr f z = foldr f z . fold . unpack
  foldl f z = foldl f z . fold . unpack

instance Traversable Staff where
  traverse f s = pack <$> (traverse (traverse f) . unpack $ s)


data Chart = Chart { chrtKey    :: Key
                   , chrtStaff  :: Staff M.Chord
                   , chrtMelody :: Staff (M.Class, M.Dur)
                   }
  deriving(Show, Eq)

