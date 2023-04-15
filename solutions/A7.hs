module A7 where

import Provided
import A6

import Data.Char ( toUpper )
import System.Directory ( doesFileExist )

-- *** A7: Functors & Applicatives *** --

-- Q#01

getUpperChar :: IO Char
getUpperChar = fmap toUpper getChar

-- Q#02

_DICT_ :: IO Dictionary
_DICT_ = lines <$> readFile _DICT_FILE_

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid (Left err) = Left err
makeGameIfValid (Right s) = Right (makeGame s) 

-- Q#04
getDict :: IO (Maybe Dictionary)
getDict = let f = toMaybe <$> doesFileExist _DICT_FILE_
            in f <*> _DICT_