module Dictionary (
    Dictionary,
    fromList,
    fromWords,
    fromWordsFiltered,
    fromFile,
    fromFileCached,
    startingWith
) where

import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Trie (Trie)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Trie as Trie

type Dictionary = Trie ()

fromList :: [ByteString] -> Dictionary
fromList list = list
    & map (\word -> (word, ()))
    & Trie.fromList

fromWords :: ByteString -> Dictionary
fromWords words = words
    & CBS.split '\n'
    & fromList

fromWordsFiltered :: ByteString -> (Dictionary, ByteString)
fromWordsFiltered = undefined

fromFile :: FilePath -> IO Dictionary
fromFile path = path
    & BS.readFile
    <&> fromWords

fromFileCached :: FilePath -> IO Dictionary
fromFileCached = undefined


startingWith :: ByteString -> Dictionary -> (Maybe (), Maybe Dictionary)
startingWith = Trie.lookupBy (\exists subDict -> (
        exists,
        Just subDict & mfilter (not . Trie.null)
    ))
