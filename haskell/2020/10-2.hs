{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import GHC.Exts (IsList (..))
import Prelude hiding (toList)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as Text
import qualified Relude.Unsafe as Unsafe

main :: IO ()
main = do
  input <- Text.getContents
  print (solve input)

solve :: Text -> Int
solve input = length arrangements
  where
    bagAdapters = input
      & lines
      & fmap (Unsafe.read @Natural . toString)
      & sort

    builtinAdapter = List.maximum bagAdapters + 3

    adapters = bagAdapters <> [builtinAdapter]

    arrangements = undefined :: [()]

















data List a
  = Nil
  | Cons a (List a)
  deriving stock Show


instance Semigroup (List a) where
  (<>) xs Nil = xs
  (<>) Nil ys = ys
  (<>) (Cons x xs) ys = Cons x (xs <> ys)


instance Monoid (List a) where
  mempty = Nil


instance IsList (List a) where
  type Item (List a) = a

  fromList :: [a] -> List a
  fromList [] = Nil
  fromList (x : xs) = Cons x (fromList xs)

  toList :: List a -> [a]
  toList Nil = []
  toList (Cons x xs) = x : toList xs


instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)


instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = Cons y (ys `undefined` ys')
    where
      Cons y ys = f x
      ys' = xs >>= f


-- $> demo
demo :: IO ()
demo = print do
  x <- Cons True (Cons False Nil)
  y <- Cons True (Cons False Nil)
  pure [x, y]


data Tree a = MkTree a [Tree a]


instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (MkTree x xts) = MkTree (f x) (fmap (fmap f) xts)


instance Applicative Tree where
  pure :: a -> Tree a
  pure x = MkTree x []

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) ft@(MkTree f _) (MkTree x xts) = MkTree (f x) (fmap (ft <*>) xts)


instance Monad Tree where
  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (>>=) (MkTree x xts) f = undefined (fmap (>>= f) xts)
