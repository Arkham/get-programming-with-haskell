module MyGuard where

import Control.Applicative
import Control.Monad (guard)

evenSteven :: Int -> [Int]
evenSteven n = do
  x <- [1 .. n]
  myGuard (even x)
  return x

listMonads :: [Int]
listMonads =
  [1 .. 10] >>=
  (\x ->
     [1 .. 10] >>=
     (\y ->
        let z = 1
         in myGuard (even x) >> return (x + 1)))
  -- x <- [1 .. 10]
  -- y <- [1 .. 10]
  -- let z = 1
  -- myGuard (even x)
  -- return (x + 1)

myGuard :: Alternative f => Bool -> f ()
myGuard True = pure ()
myGuard False = empty
--
-- guard :: GHC.Base.Alternative f => Bool -> f ()
--
-- class Applicative f => Alternative (f :: * -> *) where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a
--   some :: f a -> f [a]
--   many :: f a -> f [a]
--   {-# MINIMAL empty, (<|>) #-}
--
-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   {-# MINIMAL pure, ((<*>) | liftA2) #-}
