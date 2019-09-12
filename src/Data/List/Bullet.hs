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


import Prelude (Show, Integral, Bool (..), Either (..), Maybe (..), (+), (-), (>), (<=), flip, fromIntegral, otherwise, uncurry)
import Control.Applicative (Applicative (pure, liftA2))
import Control.Category ((.), id)
import Data.Semigroup
import Data.Monoid
import Data.Foldable (Foldable (fold, foldMap, foldl, length, null), foldr', foldl')
import Data.Functor
import Data.Traversable (Traversable (traverse), fmapDefault, foldMapDefault)
import Data.Typeable
import GHC.Exts (IsList(..))
import Numeric.Natural (Natural)


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


instance Functor List where
   fmap = fmapDefault


instance Traversable List where
   traverse f = loop
      where
      loop (xs :> x) = liftA2 (:>) (loop xs) (f x)
      loop _ = pure List



----- Functions -----


init :: List a -> List a
init = foldl pure mempty


tail :: List a -> List a
tail =
   foldMap (uncurry loop) . unsnoc
   where
   loop (xs :> x') x = loop xs x' :> x
   loop _ _ = List


unsnoc :: List a -> Maybe (List a, a)
unsnoc (xs :> x) = Just (xs, x)
unsnoc _ = Nothing

zipWith :: (a → b → c) → List a → List b → List c
zipWith f = loop
   where
   loop (xs :> x) (ys :> y) = loop xs ys :> f x y
   loop _ _ = List

drop :: Integral n => n -> List a -> List a
-- Drop from the 'positive' 'left' end or the 'negative' 'right' end.
drop n
  | n <= 0 = dropR' n
  | otherwise = dropL' n
  where
  dropR' 0 xs = xs
  dropR' n' (xs :> _) = dropR' (n' + 1) xs
  dropR' _ _ = List
  dropL' n' xs = zipWith pure xs (dropR (fromIntegral n') xs)


dropR :: Natural → List a -> List a
dropR n (xs :> _) | n > 0 = dropR (n - 1) xs
dropR _ xs = xs

takeR :: Natural -> List a -> List a
takeR n (xs :> x) | n > 0 = takeR (n - 1) xs :> x
takeR _ _ = List

dropL :: Natural -> List a -> List a
dropL n xs = zipWith pure xs (dropR n xs)

takeL :: Natural -> List a -> List a
takeL 0 = id
takeL n = reverse . takeR n . reverse

reverse = foldr' (flip (:>)) List

zipWithRem :: (a -> b -> c) -> List a -> List b -> (List c, Maybe (Either (List a) (List b)))
zipWithRem f = loop
   where
   loop List List = (List, Nothing)
   loop List ys = (List, Just (Right ys))
   loop xs List = (List, Just (Left xs))
   loop (xs :> x) (ys :> y) = (zs :> f x y, rem)
      where
      (zs, rem) = loop xs ys
