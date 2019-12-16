{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module Boggle (
    FoundWord,
    Solution,
    new,
    solve,
    totalScore
) where

import Board (Board(..))
import Dictionary (Dictionary)

import qualified Board
import qualified Dictionary as Dict

import Control.Monad (guard)
import Data.Bits (Bits, bit, setBit, testBit, bitSizeMaybe, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Maybe (fromMaybe, maybeToList, fromJust)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.String.Interpolate (i)
import Data.Word (Word8)

import Debug.Trace

import qualified Data.ByteString as BS
import qualified Data.Set as Set

type IJ = Int -- (Int, Int) packed into one Int
type PathElement = Int -- (Word8, IJ) packed into one Int
type Neighbors = [PathElement]
type BitSet = Word -- BitSet used as Bits BitSet
type Path = ([PathElement], BitSet)
type PathDictElement = (PathElement, BitSet, Dictionary)

data FoundWord = FoundWord {
    score :: Int,
    word :: ByteString,
    pathSet :: BitSet,
    path :: [IJ]
}

instance Eq FoundWord where
    a == b = word a == word b

instance Ord FoundWord where
    a `compare` b = word a `compare` word b

data Solution = Solution {
    words :: [FoundWord],
    totalScore :: Int,
    board :: Board
}

-- length to score
-- needs to be monotonically increasing
type Scorer = Int -> Int -- length to score

data Boggle = Boggle {
    board :: Board,
    scorer :: Scorer,
    get :: IJ -> Word8,
    toNeighbors :: [IJ] -> Neighbors,
    startingPathSet :: BitSet,
    startingNeighborIndices :: [IJ],
    startingNeighbors :: Neighbors,
    neighborIndices :: IJ -> [IJ],
    neighbors :: IJ -> Neighbors,
    searchIndices :: Dictionary -> BitSet -> [IJ] -> [Path],
    searchFrom :: PathDictElement -> [Path],
    toFoundWord :: Path -> FoundWord,
    solve :: Dictionary -> Solution
}

newWithScorer :: Scorer -> Board -> Boggle
newWithScorer scorer board = Boggle {
    board,
    scorer,
    get,
    toNeighbors,
    startingPathSet,
    startingNeighborIndices,
    startingNeighbors = toNeighbors $ startingNeighborIndices,
    neighborIndices,
    neighbors = toNeighbors . neighborIndices,
    searchIndices,
    searchFrom,
    toFoundWord,
    solve
}
  where
    
    Board {board = boardArray, size = (m, n)} = board
    
    fromIJ :: IJ -> (Int, Int)
    fromIJ = (`divMod` m)
    
    toIJ :: (Int, Int) -> IJ
    toIJ (!i, !j) = i * n + j
    
    get :: IJ -> Word8
    get ij = BS.index boardArray ij
    
    fromPathElement :: PathElement -> (Word8, IJ)
    fromPathElement e = (fromIntegral e .&. 0xFF, e `shiftR` 8)
    
    toPathElement :: (Word8, IJ) -> PathElement
    toPathElement (!c, !ij) = fromIntegral c .|. ij `shiftL` 8
    
    toNeighbors :: [IJ] -> Neighbors
    toNeighbors = map (\ij -> toPathElement (get ij, ij))
    
    startingPathSet :: BitSet
    startingPathSet = 0
    
    -- TODO assert this is Nothing
    bitSetSizeCheck = do
        bitSize <- bitSizeMaybe startingPathSet
        guard $ bitSize > m * n
        return [i|BitSet is not big enough for size #{(m, n)},
            but this size is intractably large anyways.|]
    
    prod s t = [(a, b) | a <- s, b <- t]
    
    startingNeighborIndices = [0..(m - 1)] `prod` [0..(n - 1)]
        & map toIJ
    
    indices = [-1..1] `prod` [-1..1]
        & filter (/= (0, 0))
    
    neighborIndices :: IJ -> [IJ]
    neighborIndices ij = indices
        & map (\(!x, !y) -> (i + x, j + y))
        & filter (\(!i, !j) -> i >= 0 && i < m && j >= 0 && j < n)
        & map toIJ
      where
        (!i, !j) = fromIJ ij
    
    searchIndices :: Dictionary -> BitSet -> [IJ] -> [Path]
    searchIndices subDict pathSet indices = indices
        & filter (not . (pathSet `testBit`))
        & toNeighbors
        & map searchNeighbor
        & concat
      where
        searchNeighbor :: PathElement -> [Path]
        searchNeighbor pathElem = currentPath ++ subPaths
          where
            (!c, !ij) = fromPathElement pathElem
            (found, maybeSubDict) = Dict.startingWith (BS.singleton c) subDict
            
            currentPath :: [Path]
            currentPath = found
                <&> const ([pathElem], pathSet)
                & maybeToList
            
            subPaths :: [Path]
            subPaths = maybeSubDict
                <&> (pathElem, pathSet `setBit` ij, )
                <&> searchFrom
                & fromMaybe []
    
    searchFrom :: PathDictElement -> [Path]
    searchFrom (!pathElem, !pathSet, !subDict) = ij
        & neighborIndices
        & searchIndices subDict pathSet
        & map (\(!path, !pathSet) -> (pathElem : path, pathSet)) -- TODO simplify
      where
        (!c, !ij) = fromPathElement pathElem
    
    toFoundWord :: Path -> FoundWord
    toFoundWord (!combinedPath, !pathSet) = FoundWord {word, pathSet, path, score}
      where
        unPackedPath = combinedPath & map fromPathElement
        word = unPackedPath & map fst & BS.pack
        path = unPackedPath & map snd
        score = scorer $ BS.length word
    
    solve dict = Solution {words, totalScore, board}
      where
        words = startingNeighborIndices
            & searchIndices dict startingPathSet
            & map toFoundWord
            & filter ((> 0) . score)
            & Set.fromList
            & Set.toList
            & sortBy cmp
        cmp = (comparing (BS.length . word)) `mappend` (comparing word)
        totalScore = words
            & map score
            & sum

new :: Board -> Boggle
new = newWithScorer scorer
  where
    scorer n
        | n < 3 = 0
        | n == 3 = 1
        | n == 4 = 1
        | n == 5 = 2
        | n == 6 = 3
        | n == 7 = 5
        | n > 7 = 11

data PrettyFoundWord = PrettyFoundWord {
    score_ :: Int,
    word_ :: ByteString,
    path :: [(Int, Int)]
} deriving Show

data PrettyFoundWords = PrettyFoundWords [PrettyFoundWord]

instance Show PrettyFoundWords where
    show (PrettyFoundWords words) = words
        & map show
        & map ('\t' : )
        & unlines
        & ('\n' : )

data PrettySolution = PrettySolution {
    words :: PrettyFoundWords,
    totalScore_ :: Int,
    board :: Board
} deriving Show

prettySolution :: Solution -> PrettySolution
prettySolution Solution {words, totalScore, board} = PrettySolution {
    words = words & map convert & PrettyFoundWords,
    totalScore_ = totalScore,
    board
}
  where
    (m, n) = Board.size board
    convert FoundWord {score, word, path} = PrettyFoundWord {
        score_ = score,
        word_ = word,
        path = path & map (`divMod` m)
    }

instance Show Solution where
    show = show . prettySolution

dict' = Dict.fromFile "data/sowpods.txt"

boggle' = ["LIST", "FROM", "WORD", "HELL"]
    & Board.fromList
    & fromJust
    & Boggle.new
