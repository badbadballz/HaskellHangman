module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isAlpha, toUpper, toLower)

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]


-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver deriving Eq

{-
instance Show GameException where
  show InvalidWord = "Invalid word"
  show InvalidMove = "Invalid move"
  show RepeatMove = "Repeat move"
  show GameOver = "Game over"
-}
-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange s = let ls = length s
                      min = fst _LENGTH_
                      max = snd _LENGTH_
                      in min <= ls && ls <= max


-- Q#04
-- is a invalid move true or false?
invalidMove :: Move -> Bool
invalidMove = isAlpha 

-- Q#05
-- char -> String -> String -> string
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m s g = zipWith (\a b -> if a == m then m else b) s g

test1 = "secret"
test1a = "______"
test2 = "helloworld"
test2a = "__________"

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c 
    | m `elem` s = c
    | otherwise = c - 1

-- Q#07

setSecret :: IO Secret
setSecret = do
              putStr "Enter a secret word:\t"
              showInput False
              s <- getLine
              showInput True
              _SPACE_
              return s

-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game { secret :: Secret,
                   guess :: Guess,
                   moves :: [Move],
                   chances :: Chances
                 } deriving Eq 


-- Q#09
repeatedMove :: Move -> Game -> Bool
repeatedMove m g = let ms = moves g
                    in m `elem` ms

-- Q#10
makeGame :: Secret -> Game
makeGame s = let s' = map (toUpper) s
                 g = replicate (length s) '_' -- how to use const instead??
                 ms = []
              in Game { secret = s', guess = g, moves = ms, chances = _CHANCES_}   

testGame = makeGame "hahaha"

-- Q#11

-- lower case and upper case letters?
updateGame :: Move -> Game -> Game
updateGame m g = let gu = revealLetters m (secret g) (guess g)
                     cu = updateChances m (secret g) (chances g)
                   in g {guess = gu, moves = m : moves g, chances = cu}   

-- Q#12

instance Show Game where
  show g = showGameHelper (secret g) (moves g) (chances g)

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13
instance Show GameException where
  show InvalidWord = "Invalid word"
  show InvalidMove = "Invalid move"
  show RepeatMove = "Repeat move"
  show GameOver = "Game over"


-- *** A6-2: Exception Contexts *** --

-- Q#14
toMaybe :: Bool -> a -> Maybe a
toMaybe True a = Just a
toMaybe False _ = Nothing  

-- Q#15
-- not sure yet what the Right return should be? should be ok
validateSecret :: (Secret -> Bool) -> Secret -> Either GameException Secret
validateSecret f s 
    | f s = Right s
    | otherwise = Left InvalidWord

-- Q#16

hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (all isAlpha) 

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange  

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict d = validateSecret (\x -> (map toLower x) `elem` d) 

-- Q#17
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = validateNoDict s >>= isInDict d 

-- Q#18
processTurn :: Move -> Game -> Either GameException Game
processTurn = undefined