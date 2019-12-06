{-# LANGUAGE OverloadedStrings #-}

module FileCounts where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Environment

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.words) input

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =
  T.pack
    (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])

-- mainOld :: IO ()
-- mainOld = do
--   args <- getArgs
--   let fileName = head args
--   input <- readFile fileName
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--   putStrLn summary
-- mainFixed :: IO ()
-- mainFixed = do
--   args <- getArgs
--   let fileName = head args
--   file <- openFile fileName ReadMode
--   input <- hGetContents file
--   let summary = (countsText . getCounts) input
--   putStrLn summary
--   hClose file
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (countsText . getCounts) input
  TI.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TI.putStrLn summary
