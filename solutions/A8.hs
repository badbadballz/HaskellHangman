module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State
import Data.Char (toUpper)
-- *** A8: Monads *** --

-- Q#01

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = validateNoDict s >>= isInDict d 

-- Q#02

playGame :: Game -> IO()
playGame g = do
              promptGuess
              c <- getUpperChar
              _SPACE_
              case processTurn c g of (Left GameOver) -> do
                                                             putStrLn $ show GameOver
                                                             putStrLn ("The correct word was " ++ secret g ++ ".") 
                                      Left err -> do
                                                    putStrLn $ show err
                                                    playGame g
                                      Right g' -> do
                                                    putStr $ show g'
                                                    if guess g' == secret g' then putStrLn "You win!"
                                                        else playGame g'

-- Q#03
startGame :: (Secret -> Either GameException Secret) -> IO()
startGame f = do
                s <- setSecret
                let v = f s
                 in 
                    case makeGameIfValid v of Left err -> do
                                                            putStrLn $ show err
                                                            startGame f
                                              Right g ->  do 
                                                            putStr $ show g
                                                            playGame g
                                    
-- Q#04

runApp :: IO ()
runApp = do
            d <- getDict
            case d of Just dict -> startGame (validateWithDict dict)
                      Nothing   -> do
                                    putStrLn "Missing dictionary file! Continue without dictionary? [Y/N]"
                                    r <- getUpperChar
                                    _SPACE_
                                    case r of 
                                               'Y'  -> startGame validateNoDict
                                               'N'  -> putStrLn "Bye bye!"
                                               _   -> runApp

    
--putStrLn "Welcome to Part II of EMURGO Academy's Haskell course!"

-- Q#05
makeGameS :: Secret -> State Game ()
makeGameS st =  put g
                    where
                          g = let s' = map (toUpper) st
                                  gs = replicate (length st) '_' -- how to use const instead??
                                  ms = []
                                  in Game { secret = s', guess = gs, moves = ms, chances = _CHANCES_} 
{-
updateGameS :: Move -> State Game ()
updateGameS m = do
                 g <- get
                 let gu = revealLetters m (secret g) (guess g)
                     cu = updateChances m (secret g) (chances g)
                     g' = g {guess = gu, moves = m : moves g, chances = cu}  
                   in put g' 
-}
updateGameS :: Move -> State Game ()
updateGameS m = modify (updateGame m)

repeatedMoveS :: Move -> State Game Bool
repeatedMoveS m = gets (repeatedMove m)
{-}
processTurn :: Move -> Game -> Either GameException Game
processTurn m g 
      | invalidMove m = Left InvalidMove
      | repeatedMove m g = Left RepeatMove
      | otherwise = if chances g' <= 0 then Left GameOver
                      else Right g'
                      where
                        g' = updateGame m g
-}
processTurnS :: Move -> State Game (Either GameException ())
processTurnS m 
        | invalidMove m = return $ Left InvalidMove
        | otherwise = do
                        r <- repeatedMoveS m -- why did runState (repeatedMoveS) return a type error??
                        if r then return $ Left RepeatMove
                        else do
                               updateGameS m
                               g' <- get
                               if chances g' <= 0 then return $ Left GameOver
                                  else return $ Right ()
                                     