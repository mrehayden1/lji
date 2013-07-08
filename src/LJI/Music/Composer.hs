{-# LANGUAGE ScopedTypeVariables #-}
module LJI.Music.Composer (

    melody
  , rootProgression
  , fifths
  , rootsFifths1
  , rootsFifths2
  , guideToneLine1
  , guideToneLine2

  , compose
  , mordent
  , target
  , mixMelodies

  , permutations

  ) where  

import Prelude hiding (mapM, minimum)

import Control.Arrow
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Error.Class
import Control.Monad.Random
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Trans
import Control.Newtype

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.List hiding (minimum)

import Debug.Trace

import LJI.Music (Tie(..), Class, Pitch, Dur, Note(..), Chord(..), (%),
                  minPitch, maxPitch, pitchClass, classDist)
import LJI.Music.Notation (Staff, Section, Chart(..))

import Data.Pointed
import qualified Data.Pointed as P (Pointed(..))

import Debug.Trace

melody, rootProgression, fifths, guideToneLine1, guideToneLine2, rootsFifths1, rootsFifths2
  :: Chart -> Staff (Class, Dur)

melody = chrtMelody

rootProgression = fmap ((,) <$> chdRoot <*> chdDur) . chrtStaff

fifths = fmap ((,) <$> chdFifth <*> chdDur) . chrtStaff

guideToneLine1 = fmap (\(gt, d) -> (fst gt, d)) . guideToneLines chdThird chdSeventh

guideToneLine2 = fmap (\(gt, d) -> (snd gt, d)) . guideToneLines chdThird chdSeventh

rootsFifths1 = fmap (\(gt, d) -> (fst gt, d)) . guideToneLines chdRoot chdFifth

rootsFifths2 = fmap (\(gt, d) -> (snd gt, d)) . guideToneLines chdRoot chdFifth


guideToneLines :: (Chord -> Class) -> (Chord -> Class) -> Chart -> Staff ((Class, Class), Dur)
guideToneLines f g = pack . fmap (flip evalState Nothing
                       . mapM (guideToneLines' f g)) . unpack . chrtStaff

guideToneLines' :: (Chord -> Class) -> (Chord -> Class) -> Chord -> State (Maybe (Class, Class)) ((Class, Class), Dur)
guideToneLines' f g ch =
  do s <- get
     let d = chdDur ch
     case s of
       Nothing ->
         do let t = f ch
                s = g ch
            put $ Just (t, s)
            return ((t, s), d)
       Just (t', s') ->
         do let t = f ch
                s = g ch
            if (classDist t t' + classDist s s')
                 <= (classDist t s' + classDist t' s)
              then put (Just (t, s)) >> return ((t, s), d)
              else put (Just (s, t)) >> return ((s, t), d)

target :: Dur -> [Int] -> Note -> Note -> [Note]
target dt ts (Note p d) (Note p' _) =
  let (ts', d') = targeting dt ts d
  in (if d' > 0 then [Note p d'] else [])
       ++ map (flip Note dt . (p'+)) ts'

mordent :: Dur -> [Int] -> Note -> [Note]
mordent dt ts (Note p d) =
  let (ts', d') = targeting dt ts d
  in map (flip Note dt . (p+)) ts'
      ++ (if d' > 0 then [Note p d'] else [])

-- targeting - Given a targeting spec (duration and chromatics) return the
-- targeting that can be applied within the given duration along with what
-- remains 
--
-- If the target note is not long enough return the longest possible tail of 
-- the target notes
targeting :: Dur -> [Int] -> Dur -> ([Int], Dur)
targeting _  [] d = ([], d)             -- True anyway, but not clear
targeting dt ts d | dt >= d   = ([], d)
                  | otherwise =
  let (ts', d') = head . dropWhile ((>=d) . snd) . map (flip (,) =<< dur)
                    . tails $ ts
  in (ts', d - d')
 where
  dur = (*dt) . (%1) . fromIntegral . length

compose :: RandomGen g =>
            (Pitch, Pitch)
            -> (Class, Dur)
            -> StateT (Maybe Pitch) (Rand g) Note
compose (l, h) (c, d) = do 
  p  <- get
  p' <- case p of 
    Nothing ->
      lift
        . randomChoice
        . filter (\x -> pitchClass x == c
                          && l <= x
                          && x <= h
                 )
        $ [l..h]
    Just p  ->
      lift
        . randomChoice
        . filter (\x -> pitchClass x == c
                          && l <= x
                          && x <= h
                          && abs (x - p) < 12
                 )
       $ [l..h]
  put . Just $ p'
  return $ Note p' d

randomChoice :: RandomGen g => [a] -> Rand g a
randomChoice [] = fail "LJI: Internal error (randomChoice: emptyList)"
randomChoice as = do (n :: Int) <- getRandomR (0, length as - 1)
                     return $ as !! n



-- mixMelodies - deterministically mix two or more lists to create a new one
-- from segments of length n from each. We take the first segment from the 
-- first melody and the second from the second and so on section by section
-- until there is nothing left.
mixMelodies :: Dur -> [Staff (a, Dur)] -> [Staff (a, Dur)]
mixMelodies d = map (mixMelodies' d) . permutations
 where
  mixMelodies' :: Dur -> [Staff (a, Dur)] -> Staff (a, Dur)
  mixMelodies' _ []  = pack []
  mixMelodies' d xs  =
    pack . map (mixSections d) . transpose . map unpack $ xs

mixSections :: Dur -> [Section (a, Dur)] -> Section (a, Dur)
mixSections d = pack . mixSections' d . map unpack
 where

  mixSections' :: Dur -> [[(a, Dur)]] -> [(a, Dur)]
  mixSections' _ []       = []
  mixSections' _ ([]:_)   = []
  mixSections' d (xs:xss) =
    let (xs', rest) = takeMelody d xs
        rests       = map (snd . takeMelody d) xss
    in xs' ++ mixSections' d (rests ++ [rest])
  
takeMelody :: Dur -> [(a, Dur)] -> ([(a, Dur)], [(a, Dur)])
takeMelody _ []            = ([], [])
takeMelody d ((a,da):rest) =
  case da `compare` d of
    EQ -> ([(a, da)], rest)
    LT -> let (as, rest') = takeMelody (d-da) rest
          in ((a, da):as, rest')
    GT -> ([(a, d)], (a, da-d):rest)

