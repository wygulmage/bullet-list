{-# LANGUAGE TypeFamilies #-}


module Data.List.Bullet
   ( List (..)
   , (•)
   ) where


import Control.Applicative (Applicative (pure, liftA2))
import Data.Foldable (Foldable (foldl), foldr', foldl')
import Data.Traversable (Traversable, fmapDefault, foldMapDefault)
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
   (<>) xs = loop
      where
      loop (ys :> y) = loop ys :> y
      loop _ = xs

instance Monoid (List a) where
   mempty = List


instance Functor List where
   fmap = fmapDefault
   -- fmap f (xs :> x) = fmap f xs :> f x
   -- fmap _ _ = List

instance Foldable List where
   foldl f z = loop
      where
      loop (xs :> x) = loop xs `f` x
      loop _ = z
   foldMap f = foldl (flip (flip (<>) . f)) mempty

instance Traversable List where
   traverse f = loop
      where
      loop (xs :> x) = liftA2 (:>) (loop xs) (f x)
      loop _ = pure List
