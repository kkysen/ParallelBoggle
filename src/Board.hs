{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Board (
    Size,
    Board(..),
    fromRaw,
    fromSizedList,
    fromList,
    replaceLetter,
    swapLetters
) where

import Metric (MetricSpace, metric2)

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Vector.Storable (Vector, Storable, MVector, modify)
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Vector.Storable.Mutable (swap, write)
import Data.Word (Word8)
import System.Random (RandomGen, randomRs)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List

type IJ = Int -- compressed two (Int32, Int32)
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

instance Show Board where
    show Board {board, size = (m, n)} = show (m, n) ++ "\n" ++ boardString
      where
        boardString = [0..(m - 1)]
            & map (* n)
            & map (\start -> (BS.take n . BS.drop start) board)
            & List.intersperse (BSC.singleton '\n')
            & BS.concat
            & BSC.unpack

instance MetricSpace Board where
    metric2 Board {board = a} Board {board = b} = BS.zip a b
          & map (\(x, y) -> (c x, c y))
          & map (\(x, y) -> x - y)
          & map (\x -> x * x)
          & sum
          & fromIntegral
        where
          c :: Word8 -> Int
          c = fromIntegral

type Map a = a -> a

as :: (a -> b) -> (b -> a) -> Map b -> Map a
as to from f x = x & to & f & from

bsAsVecs = as byteStringToVector vectorToByteString

boardAsVecs f b@Board {board} = b {board = bsAsVecs f board}

replaceLetter :: IJ -> Word8 -> Map Board
replaceLetter i c = boardAsVecs $ modify (\v -> write v i c)

swapLetters :: IJ -> IJ -> Map Board
swapLetters i j = boardAsVecs $ modify (\v -> swap v i j)
