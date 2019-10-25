module Main where

import Control.Monad
import Data.Int (Int)
import Debug.Trace
import Prelude
import System.Environment
import System.IO (IO)

main :: IO ()
main = mainQuote

mainPut :: IO ()
mainPut = do
  args <- getArgs
  mapM_ putStrLn args

mainNonLazy :: IO ()
mainNonLazy = do
  args <- getArgs
  let linesToRead =
        if length args > 0
          then read (head args)
          else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

toInts :: String -> [Int]
toInts = map read . lines

mainLazy :: IO ()
mainLazy = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

mainLazySquare :: IO ()
mainLazySquare = do
  userInput <- getContents
  let numbers = toInts userInput
  print ((sum . map (^ 2)) numbers)

solve :: String -> String
solve line = solve' (words (trace ("line: " ++ show line) line))
  where
    solve' [first, op, second] =
      solve'' (read first :: Int) op (read second :: Int)
    solve' _ = "error"
    solve'' first "*" second = show (first * second)
    solve'' first "+" second = show (first + second)
    solve'' _ _ _ = "error"

mainSimpleCalc :: IO ()
mainSimpleCalc = do
  userInput <- getContents
  mapM_ (print . solve) (lines userInput)

getQuote :: [String] -> [String]
getQuote ("1":xs) = "To be or not to be" : getQuote xs
getQuote ("2":xs) = "My kingdom for a horse" : getQuote xs
getQuote ("3":xs) = "Girls just wanna have fun" : getQuote xs
getQuote ("4":xs) = "NOOOOOOOOOOOOOOOOOOOOOOOOOO" : getQuote xs
getQuote ("5":xs) = "That's a bingo" : getQuote xs
getQuote ("n":_) = ["Bye!"]
getQuote (_:xs) = getQuote xs
getQuote [] = []

mainQuote :: IO ()
mainQuote = do
  putStrLn "Which quote do you want? 1-5 to choose, n to exit."
  userInput <- getContents
  mapM_ putStrLn (getQuote (lines userInput))
