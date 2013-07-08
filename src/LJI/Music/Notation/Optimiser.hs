{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LJI.Music.Notation.Optimiser ( 
    optimalNotation
  ) where

import Prelude hiding (mapM)

import Control.Monad hiding (mapM)
import Control.Monad.Trans
import Control.Monad.State hiding (mapM)

import Data.Traversable

import qualified LJI.Music as M
import LJI.Music (Enharm(..))
import qualified LJI.Music.Notation as N (Key, Note, Class(..), Name(..))
import LJI.Music.Notation (Key(..), Note(..), Pitch(..))
import LJI.Music.Notation.Scales (Direction(..), chromaticScale)

newtype StateList s a = 
  StateList { unwrapStateList :: StateT s [] a }
    deriving (Monad, MonadState s, MonadPlus)

liftChoice :: [a] -> StateList s a
liftChoice = StateList . lift

type Optimiser a = StateList OptimiserState a

data OptimiserState = OS { osDirection :: Direction
                         , osLastNote  :: Maybe M.Note
                         }

runOptimiser :: OptimiserState -> Optimiser a -> [a]
runOptimiser s = flip evalStateT s . unwrapStateList

optimalNotation :: Traversable f => N.Key -> f M.Note -> f N.Note
optimalNotation k ns =
  let ntdirs = head $ do d <- [Desc, Asc]
                         runOptimiser (OS d Nothing)
                           . mapM (optimiser k)
                           $ ns
  in fmap (uncurry (notate k)) ntdirs

notate :: Key ->  M.Note -> Direction -> N.Note
notate k n d = let (o, c) = M.ntPitch n `divMod` 12
                   ec'    = (!! c) `liftM` chromaticScale k d
                   c'     = either error id ec'
                   o'     = case c' of
                              N.FlatFlat   N.C -> o + 1
                              N.Flat       N.C -> o + 1
                              N.Sharp      N.B -> o - 1
                              N.SharpSharp N.B -> o - 1
                              otherwise        -> o
               in Note (Pitch c' o') (M.ntDur n)

optimiser :: N.Key -> M.Note -> Optimiser (M.Note, Direction)
optimiser k n@(M.Note q _) = 
  do p <- gets (maybe q M.ntPitch . osLastNote)
     d <- gets osDirection
     if p == q
       then liftChoice [(n, d)]
       else do guard (direction p q == d)
               (n, d) <- liftChoice [(n, Desc), (n, Asc)]
               put $ OS d (Just n)
               return (n, d)
 where
  direction :: M.Pitch -> M.Pitch -> Direction
  direction p q | p < q     = Asc
                | p > q     = Desc
                | otherwise = error "optimiser: internal error"
