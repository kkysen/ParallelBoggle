{-# LANGUAGE NamedFieldPuns #-}

module Board (
    Size,
    Board(..),
    fromRaw,
    fromSizedList,
    fromList,
    unwrap
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List

type Size = (Int, Int)

data Board = Board {
    board :: ByteString,
    size :: Size
}

fromRaw :: Size -> ByteString -> Maybe Board
fromRaw size board = do
    let (m, n) = size
    guard $ BS.length board == m * n
    return Board {board, size}

fromSizedList :: Size -> [ByteString] -> Maybe Board
fromSizedList size rows = do
    let (m, n) = size
    guard $ length rows == m
    guard $ rows & map BS.length & all (== n)
    return Board {board = BS.concat rows, size}

fromList :: [ByteString] -> Maybe Board
fromList rows = fromSizedList size rows
  where
    size = (m, n)
    m = length rows
    n = BS.length $ head rows

unwrap :: Maybe Board -> Board
unwrap = fromJust

instance Show Board where
    show Board {board, size = (m, n)} = show (m, n) ++ "\n" ++ boardString
      where
        boardString = [0..(m - 1)]
            & map (* n)
            & map (\start -> (BS.take n . BS.drop start) board)
            & List.intersperse (BSC.singleton '\n')
            & BS.concat
            & BSC.unpack
