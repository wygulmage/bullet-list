{-# LANGUAGE
    UnicodeSyntax
  , NoImplicitPrelude
  , BangPatterns
  , TypeFamilies #-}


module Data.List.Bullet
   ( List (..)
   , (•)
   , init, tail, unsnoc
   ) where


import Prelude (Show, Bool (..), Maybe (..), (+), maybe, uncurry)
import Control.Applicative (Applicative (pure, liftA2))
import Control.Category ((.))
import Data.Semigroup
import Data.Monoid
import Data.Foldable (Foldable (fold, foldMap, foldl, length, null), foldr', foldl')
import Data.Functor
import Data.Traversable (Traversable (traverse), fmapDefault, foldMapDefault)
import Data.Typeable
import GHC.Exts (IsList(..))


-- A snoc list:
data List a
   = List
   | List a :> a
   deriving (Typeable, Show)

infixl 5 :>


(•) :: List a -> a -> List a
(•) = (:>)
infixl 1 •


-- Example:
-- items = List
--    • item1
--    • item2
--    • item3

----- Instances -----

instance IsList (List a) where
   type Item (List a) = a

   toList = foldr' (:) []

   fromList = foldl' (:>) List


instance Semigroup (List a) where
   (<>) = foldl (:>)


instance Monoid (List a) where
   mempty = List


instance Functor List where
   fmap = fmapDefault


instance Foldable List where
   fold = foldl (<>) mempty

   foldMap f = foldl (\ x y -> x <> f y) mempty -- Compare to foldr ((<>) . f) mempty.

   foldl f z = loop
      where
      loop (xs :> x) = loop xs `f` x
      loop _ = z

   length =
      foldr' (pure (+1)) 0

   null List = True
   null _ = False


instance Traversable List where
   traverse f = loop
      where
      loop (xs :> x) = liftA2 (:>) (loop xs) (f x)
      loop _ = pure List



----- Functions -----

init :: List a -> List a
init = foldl pure mempty
-- init (xs :> _) = xs
-- init _ = List

tail :: List a -> List a

tail = maybe mempty (uncurry loop) . unsnoc
   where
   loop (xs :> x') x = loop xs x' :> x
   loop _ _ = List


unsnoc :: List a -> Maybe (List a, a)
unsnoc (xs :> x) = Just (xs, x)
unsnoc _ = Nothing
