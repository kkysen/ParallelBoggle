{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Lang (
    Dict,
    Lang,
    dict,
    fromWords,
    fromFile,
    startingWith,
    randomLetters,
    randomLetter
) where

import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Trie (Trie)
import Data.Word (Word8)
import System.IO.MMap (mmapFileByteString)
import System.Random (RandomGen, randomRs)
import Control.Monad.Random.Class (MonadRandom, getRandomRs)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Trie as Trie

import Debug.Trace

type Dict = Trie ()

-- All the bytes of the dictionary file are stored explicitly,
-- because this allows for simple frequency-based random lookups in O(1),
-- and b/c the Trie probably already stores ByteString views of this main string,
-- and even if it doesn't, the Trie's memory dwarfs the raw string memory.
data Lang = Lang {
    dict :: Dict,
    bytes :: ByteString
}

fromWords :: ByteString -> Lang
fromWords bytes = Lang {dict, bytes}
  where
    dict = bytes
        & CBS.split '\n'
        & map (, ())
        & Trie.fromList

fromFile :: FilePath -> IO Lang
fromFile path = mmapFileByteString path Nothing
    <&> fromWords

startingWith :: ByteString -> Dict -> (Maybe (), Maybe Dict)
startingWith = Trie.lookupBy (\exists subDict -> (
        exists,
        Just subDict & mfilter (not . Trie.null)
    ))

-- randomRsState :: (RandomGen g, Random r) => (r, r) -> g ->
--
-- randomLetter' :: RandomGen g => State g Lang -> State g Word8
-- randomLetter' s = do
--     g <- get s

randomLetters :: MonadRandom m => Lang -> Int -> m ByteString
randomLetters Lang {bytes} n = getRandomRs (0, BS.length bytes - 1)
    <&> map (BS.index bytes)
    <&> filter (/= c2w '\n')
    <&> take n
    <&> BS.pack

randomLetter :: MonadRandom m => Lang -> m Word8
randomLetter lang = do
    letters <- randomLetters lang 1
    return $ BS.index letters 0
