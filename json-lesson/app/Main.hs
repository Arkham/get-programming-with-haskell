module Main where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.Text as T
import GHC.Generics

-- deriving generic

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Ju", title = "Bottle", year = 2020 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

bookFromJSON :: Maybe Book
bookFromJSON = decode myBookJSON

-- manually

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage m e) =
    object [ "message" .= m
           , "error" .= e
           ]

-- NOAA
data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Float
  , resultId :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
    object [ "uid" .= uid
           , "mindate" .= mindate
           , "maxdate" .= maxdate
           , "name" .= name
           , "datacoverage" .= datacoverage
           , "id" .= resultId
           ]

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset
instance ToJSON Resultset

data Metadata = Metadata
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left err) = print err
printResults (Right results) = do
  forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

exerciseOne :: IO ()
exerciseOne = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  print $ encode noaaResponse

data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList

exerciseTwo :: IO ()
exerciseTwo = do
  print $ encode intListExample
