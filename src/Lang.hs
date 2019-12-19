{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Lang (
    Dict,
    Lang(..),
    size,
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
import Data.Maybe (fromMaybe)
import Data.Trie (Trie)
import Data.Trie.Internal (Trie(..))
import Data.Word (Word8)
import System.IO.MMap (mmapFileByteString)
import System.Random (RandomGen, randomRs)
import Control.Monad.Random.Class (MonadRandom, getRandomRs)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Trie as Trie
import qualified Data.Trie.Internal as TrieInternal

import Debug.Trace

withSizes :: (Int -> a -> b) -> Trie a -> (Trie b, Int)
withSizes map = f
  where
    f Empty = (Empty, 0)
    f (Arc k Nothing t) = (Arc k Nothing t', n)
      where
        (t', n) = f t
    f (Arc k (Just v) t) = (Arc k (Just $! map (n + 1) v) t', n + 1)
      where
        (t', n) = f t
--     f (Arc k v trie) = (Arc k (v <&> map (n + 1)) subTrie, n)
--       where
--         (subTrie, n) = f trie
    f (Branch p mask l r) = (Branch p mask l' r', m + n)
      where
        (l', m) = f l
        (r', n) = f r

ofSizes :: Trie a -> (Trie Int, Int)
ofSizes = withSizes (\n _ -> n)

type Dict = Trie Int

-- All the bytes of the dictionary file are stored explicitly,
-- because this allows for simple frequency-based random lookups in O(1),
-- and b/c the Trie probably already stores ByteString views of this main string,
-- and even if it doesn't, the Trie's memory dwarfs the raw string memory.
data Lang = Lang {
    dict :: Dict,
    dictSize :: Int,
    bytes :: ByteString
}

fromWords :: ByteString -> Lang
fromWords bytes = Lang {dict, dictSize, bytes}
  where
    (dict, dictSize) = bytes
        & CBS.split '\n'
        & map (, ())
        & Trie.fromList
        & ofSizes

fromFile :: FilePath -> IO Lang
fromFile path = mmapFileByteString path Nothing
    <&> fromWords

size :: Dict -> Int
size = f
  where
    f Empty = 0
    f (Arc _ Nothing t) = f t
    f (Arc _ (Just n) _) = n
    f (Branch _ _ l r) = (f l) + (f r)

startingWith :: ByteString -> Dict -> (Maybe (), Maybe Dict)
startingWith = Trie.lookupBy (\exists subDict -> (
        exists <&> const (),
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
