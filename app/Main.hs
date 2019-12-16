{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Board
import qualified Boggle
import qualified Dictionary as Dict

import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
    dict <- Dict.fromFile "data/sowpods.txt"
    let board_ = ["LIST", "FROM", "WORD", "HELL"]
--    let board_ = ["AB", "CD"]
    let board = Board.fromList board_ & fromJust
    let boggle = Boggle.new board
    print board
    let solution = Boggle.solve boggle dict
    print solution
