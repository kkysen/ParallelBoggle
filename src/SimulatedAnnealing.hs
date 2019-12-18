{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimulatedAnnealing (
   State,
   energy,
   metric,
   perturb,
   Args(..),
   CoolingTemp(..),
   anneal
) where

import Metric (MetricSpace, Metric, metric)

import qualified Metric

import Prelude hiding (min)

import Control.Monad.Random (evalRand)
import Control.Monad.Random.Class (MonadRandom, getRandom)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Numeric.GSL.SimulatedAnnealing (SimulatedAnnealingParams(..), simanSolve)
import Numeric.LinearAlgebra.Data (Vector, (!))
import System.Random (mkStdGen, StdGen)

import Debug.Trace

class MetricSpace a => State a where
    energy :: a -> Double
    perturb :: MonadRandom m => Double -> a -> m a

data Args = Args {
    numTries :: Word,
    numItersPerTemp :: Word,
    maxStepSize :: Double,
    boltzmannK :: Double,
    coolingTemp :: CoolingTemp
}

data CoolingTemp = CoolingTemp {
    initial :: Double,
    rate :: Double,
    min :: Double
}

argsToParams :: Args -> SimulatedAnnealingParams
argsToParams Args {
    numTries,
    numItersPerTemp,
    maxStepSize,
    boltzmannK,
    coolingTemp = CoolingTemp {
        initial,
        rate,
        min
    }
} = SimulatedAnnealingParams {
    n_tries = fromIntegral numTries,
    iters_fixed_T = fromIntegral numItersPerTemp,
    step_size = maxStepSize,
    boltzmann_k = boltzmannK,
    cooling_t_initial = initial,
    cooling_mu_t = rate,
    cooling_t_min = min
}

data Annealer a = Annealer {
    seed :: Int,
    numRandoms :: Int,
    params :: SimulatedAnnealingParams,
    initialState :: a,
    energy' :: a -> Double,
    metric' :: a -> a -> Double,
    step :: Vector Double -> Double -> a -> a,
    maybeShow :: Maybe (a -> String)
}

rawAnneal :: Annealer a -> a
rawAnneal Annealer {
    seed,
    numRandoms,
    params,
    initialState,
    energy',
    metric',
    step,
    maybeShow
} = simanSolve
    seed
    numRandoms
    params
    initialState
    energy'
    metric'
    step
    maybeShow

anneal :: (State a, MonadRandom m) => a -> Args -> Maybe (a -> String) -> m a
anneal initialState args maybeShow = do
    seed <- getRandom
    Annealer {
        seed,
        numRandoms = 1,
        params = argsToParams args,
        initialState,
        energy' = energy,
        metric' = metric,
        step,
        maybeShow
    }
        & rawAnneal
        & return
      where
        -- It's too hard to thread the monad through the SA,
        -- b/c the FFI library isn't designed with monads.
        -- So I'm doing it manually here, seeding a new StdGen using mkStdGen
        -- with a seed from the MonadRandom.
        step vectors stepSize s = s'
          where
            seed = (vectors ! 0)
                * (fromIntegral (maxBound :: Int32))
                & truncate
            g = mkStdGen seed
            rand = perturb stepSize s
            s' = evalRand rand g
