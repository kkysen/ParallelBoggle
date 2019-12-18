{-# LANGUAGE NamedFieldPuns #-}

module BoggleState (
    boggle,
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
    count :: Int,
    parallel :: Bool
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
    
    perturb stepSize s = multiple (ceiling stepSize) s
      where
        multiple n s
            | n > 0 = once s >>= multiple (n - 1)
            | otherwise = return s
        
        once s@BoggleState {boggle, lang, count, parallel} = do
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
                boggle = Boggle.new (modify board) parallel,
                count = count + 1
            }
            return $ s'

random :: MonadRandom m => Size -> Lang -> Bool -> m BoggleState
random size@(m, n) lang parallel = do
    board <- Lang.randomLetters lang (m * n)
    return $ BoggleState {
        boggle = Boggle.new Board {board, size} parallel,
        lang,
        count = 0,
        parallel
    }

optimize :: MonadRandom m => Size -> Lang -> Args -> Bool -> m BoggleState
optimize size lang args parallel = do
    state <- random size lang parallel
    let (m, n) = size
    let mn = m * n
    let Args {maxStepSize} = args
    let args' = args {
        maxStepSize = maxStepSize * (sqrt $ fromIntegral mn),
        boltzmannK = 1.0
    }
    SA.anneal state args' (Just $ show . count)
