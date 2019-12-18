{-# LANGUAGE NamedFieldPuns #-}

module BoggleState (
    board',
    score,
    count,
    random,
    optimize
) where

import Boggle (Boggle)
import Board (Board(..), Size)
import Lang (Lang, Dict)
import Metric (MetricSpace, metric)
import SimulatedAnnealing (Args(..), CoolingTemp(..), energy, perturb)

import qualified Board
import qualified Boggle
import qualified Lang
import qualified Metric
import qualified SimulatedAnnealing as SA

import Prelude hiding (min)

import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import Data.Function ((&))
import System.Random (RandomGen)

import Debug.Trace

data BoggleState = BoggleState {
    boggle :: Boggle,
    lang :: Lang,
    count :: Int
}

instance Show BoggleState where
    show BoggleState {boggle, count} = show count

board' :: BoggleState -> Board
board' = Boggle.board . boggle

score :: BoggleState -> Int
score BoggleState {boggle, lang} = Boggle.solve boggle (Lang.dict lang)
    & Boggle.totalScore

instance MetricSpace BoggleState where
    metric = Metric.by board' metric

instance SA.State BoggleState where
    
    energy = fromIntegral . negate . score
    
    perturb stepSize s@BoggleState {boggle, lang, count} = do
        let board = Boggle.board boggle
        let (m, n) = Board.size board
        let size = m * n
        swapOrReplace <- getRandom
        ij <- getRandomR (0, size - 1)
        modify <- case swapOrReplace of
            True -> do
                c <- Lang.randomLetter lang
                return $ Board.replaceLetter ij c
            False -> do
                ij' <- getRandomR (0, size - 1)
                return $ Board.swapLetters ij ij'
        let s' = s {
            boggle = Boggle.new $ modify board,
            count = count + 1
        }
        return $ traceShowId $ s'

random :: MonadRandom m => Size -> Lang -> m BoggleState
random size@(m, n) lang = do
    board <- Lang.randomLetters lang (m * n)
    return $ BoggleState {
        boggle = Boggle.new Board {board, size},
        lang,
        count = 0
    }

optimize :: MonadRandom m => Size -> Lang -> m BoggleState
optimize size lang = do
    state <- random size lang
    let args = Args {
        numTries = 200,
        numItersPerTemp = 1000,
        maxStepSize = 1.0,
        boltzmannK = 1.0,
        coolingTemp = CoolingTemp {
            initial = 0.008,
            rate = 1.003,
            min = 2.0e-6
        }
    }
    SA.anneal state args (Just $ show . count)
