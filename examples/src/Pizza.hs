{-# LANGUAGE OverloadedStrings #-}

module Pizza where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter d = radius * radius * pi
  where
    radius = d / 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

cheaperPizza :: Pizza -> Pizza -> Pizza
cheaperPizza p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

describeBetterPizza :: Pizza -> String
describeBetterPizza pizza@(pizzaSize, _) =
  "The " ++
  show pizzaSize ++
  " pizza is cheaper at " ++ show pizzaCostPerInch ++ " per square inch"
  where
    pizzaCostPerInch = costPerInch pizza

main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  pizza1Size <- getLine
  putStrLn "What is the cost of pizza 1?"
  pizza1Cost <- getLine
  let pizza1 = (read pizza1Size, read pizza1Cost)
  putStrLn "What is the size of pizza 2?"
  pizza2Size <- getLine
  putStrLn "What is the cost of pizza 2?"
  pizza2Cost <- getLine
  let pizza2 = (read pizza2Size, read pizza2Cost)
  let betterPizza = cheaperPizza pizza1 pizza2
  putStrLn (describeBetterPizza betterPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = cheaperPizza pizza1 pizza2
  return (describeBetterPizza betterPizza)

-- Exercise
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

helloMain :: IO ()
helloMain = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

-- Exercise with Data.Text
helloPersonT :: T.Text -> T.Text
helloPersonT name = "Hello " <> name <> "!"

helloMainT :: IO ()
helloMainT = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPersonT name
  TIO.putStrLn statement
