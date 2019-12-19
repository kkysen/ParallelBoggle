{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import SimulatedAnnealing (Args(..), CoolingTemp(..))

import qualified Board
import qualified Boggle
import qualified BoggleState
import qualified Lang

import Prelude hiding (min)

import Data.Function ((&))
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Random (getStdGen)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

test :: IO ()
test = do
    lang <- Lang.fromFile "data/sowpods.txt"
    let board_ = ["SERS", "PATG", "LINE", "SERS"]
--     let board_ = ["LIST", "FROM", "WORD", "HELL"]
--    let board_ = ["AB", "CD"]
    let board = Board.fromList board_ & fromJust
    let boggle = Boggle.new board False
    print board
    let solution = Boggle.solve boggle lang
    print solution

userTest :: IO ()
userTest = do
    lang <- Lang.fromFile "data/sowpods.txt"
    args <- getArgs
    let (board, score) = solve lang args
    print board
    print score
  where
    
    grow :: Word -> [String] -> [String]
    grow 0 args = args
    grow n args = (args ++ args) & map (\s -> s ++ s) & grow (n - 1)
    
    solve lang args = (board, score)
      where
        board = args
            & grow 3
            & map BSC.pack
            & Board.fromList
            & fromJust
        score = board
            & (`Boggle.new` False)
            & (`Boggle.solve` lang)
            & Boggle.totalScore

randomTest :: IO ()
randomTest = do
    lang <- Lang.fromFile "data/sowpods.txt"
    args <- getArgs
    let [m, n, numBoards] = args & map read
    [1..numBoards]
        & map (\_ -> run (m, n) lang)
        & sequence_
  where
    run size lang = do
        state <- BoggleState.random size lang False
        state
            & BoggleState.score
            & print

optimizeTest :: IO ()
optimizeTest = do
    lang <- Lang.fromFile "data/sowpods.txt"
    args <- getArgs
    let [m', n', numTries', numItersPerTemp', rate', min'] = args
    let [m, n] = [m', n'] & map read
    let [numTries, numItersPerTemp] = [numTries', numItersPerTemp'] & map read
    let [rate, min] = [rate', min'] & map read
    let args = Args {
            numTries,
            numItersPerTemp,
--             numTries = 10,
--             numItersPerTemp = 500,
            maxStepSize = 1.0,
            boltzmannK = 1.0,
            coolingTemp = CoolingTemp {
                initial = 1.0,
                rate,
                min
--                 rate = 1.05,
--                 min = 0.69
            }
    }
    state <- BoggleState.optimize (m, n) lang args False
    print $ BoggleState.board' state
    print $ BoggleState.score state
    print $ BoggleState.count state

main :: IO ()
main = optimizeTest
