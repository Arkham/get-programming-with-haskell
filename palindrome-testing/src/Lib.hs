module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char (isLetter, isPunctuation, isSpace)
import Data.Text as T

preprocess :: T.Text -> T.Text
preprocess str =
  T.toLower $ T.filter isLetter str

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
