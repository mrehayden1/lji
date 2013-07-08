{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module LJI.Run (
  run,
  Environment,
  runEnvironment
) where

import Prelude hiding (sequence, mapM, minimum, maximum, foldr)

import Control.Arrow
import Control.Applicative
import Control.Monad hiding (sequence, mapM)
import Control.Monad.Error hiding (sequence, mapM)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Trans
import Control.Newtype

import Data.Char
import Data.Foldable
import Data.List hiding (minimum, maximum)
import Data.Monoid
import Data.Pointed
import Data.Traversable

import System.Environment
import System.Exit
import System.IO

import LJI.Args (Options(..), Range)
import LJI.Console (putStringAndExit, putErrorAndExit, promptConfirmation')

import LJI.Music (Class, Pitch, Dur, Note(..), Chord(..), (%), Tie(..), Enharm(..))

import LJI.Music.Composer (compose, mordent, target, mixMelodies)
import LJI.Music.Notation (Key, Staff(..), Section(..))
import LJI.Music.Notation.Optimiser (optimalNotation)

import LJI.Output (melodyToLy)
import LJI.Parser (Chart(..), parseChart)

import Debug.Trace

-- Types

newtype Environment a = Env { unwrapEnvironment :: ReaderT Options IO a }
  deriving (Functor, Applicative, Monad, MonadReader Options, MonadIO)

runEnvironment :: Options -> Environment a -> IO a
runEnvironment os = flip runReaderT os . unwrapEnvironment

-- Top level functions

run :: Environment ()
run = do
  src   <- asks optSource
  input <- liftIO src

  chart <- parseChart input
  let key = chrtKey chart

  mels  <- makeMelodies chart

  ly    <- generateLilypond key mels
  args  <- asks optArgString
  let text = "\\markup{Generated by Linear Jazz Improvisation invoked with arguments \"lji " ++ args ++ "\"}"

  dest  <- asks optOutput
  liftIO $ dest (ly ++ text)

  return ()

generateLilypond :: Key -> [Staff Note] -> Environment String
generateLilypond key mels = do
  opts <- ask
  let clef      = optClef opts
      nottdMels = map (optimalNotation key) mels
      ly        = mconcat . map (melodyToLy key clef) $ nottdMels
  return ly

makeMelodies :: Chart -> Environment [Staff Note]
makeMelodies chart = do
  opts <- ask
  let melFns  = optMelodyFns opts :: [Chart -> Staff (Class, Dur)]
      ts      = optTargets   opts :: [[Int]]
      absMels = (case optMix opts of
                   Just d  -> map tieRepeats . mixMelodies d
                                . map ($ chart) $ melFns
                   Nothing -> map (tieRepeats . ($ chart)) melFns
                ) :: [Staff (Class, Dur)]
  case ts of
    [] -> mapM (generateMelody []) $ absMels
    _  -> liftM mconcat $ sequence
            [ do ns  <- mapM (generateMelody t) $ absMels
                 ns' <- mapM (targetMelody t) ns
                 return ns'
            | t <- ts ]

tieRepeats :: (Enharm a, Tie a) => Staff a -> Staff a
tieRepeats = pack . map (flip evalState Nothing . tieRepeats') . unpack
 where
  tieRepeats' :: (Enharm a, Tie a) => Section a -> State (Maybe a) (Section a)
  tieRepeats' = fmap (pack . join) . doTie . unpack

  doTie :: (Enharm a, Tie a) => [a] -> State (Maybe a) [[a]]
  doTie []     =
    do mla <- get
       case mla of
         Nothing -> return []
         Just a  -> return [[a]]
  doTie (a:as) =
    do mla <- get
       a' <- case mla of
               Nothing -> do { put (Just a) ; return [] }
               Just la -> if a `enharm` la
                            then do { put (Just (la `tie` a)) ; return [] }
                            else do { put (Just a) ; return [la] }
       as' <- doTie as
       return (a':as')

mordentMelody :: [Int] -> Staff Note -> Environment (Staff Note)
mordentMelody t ns = do
  d <- asks optDur
  return
    . over WrapStaff (fmap fold)
    . fmap (fromList . mordent d t)
    $ ns

targetMelody :: [Int] -> Staff Note -> Environment (Staff Note)
targetMelody ts ns = do
  d <- asks optDur
  let nss = flip evalState (drop 1 . cycle . toList $ ns)
             . mapM (targetMelody' d ts)
             $ ns
  return . over WrapStaff (fmap fold) $ nss
 where
  targetMelody' :: Dur -> [Int] -> Note -> State [Note] (Section Note)
  targetMelody' d ts n = do 
    ns <- gets (target d ts n . head)
    modify (drop 1)
    return . fromList $ ns

generateMelody :: [Int]
                   -> Staff (Class, Dur)
                   -> Environment (Staff Note)
generateMelody ts absMel =
  do rng     <- asks optRange
     d       <- asks optDur
     (lo,hi) <- asks optRange
     let tLow            = minimum (0:ts)
         tHi             = maximum (0:ts)
         rng'@(lo', hi') = ( lo - tLow
                           , hi - tHi  )
     liftIO . when (hi' - lo' < 11) .
       putErrorAndExit $ "Insufficient notes in range "
                           ++ "to mordent melody with sequence "
                           ++ show ts
     liftIO
       . evalRandIO
       . flip evalStateT Nothing
       . mapM (compose rng')
       $ absMel
