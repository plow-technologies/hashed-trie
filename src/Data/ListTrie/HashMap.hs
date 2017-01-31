{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ListTrie.HashMap
  ( StrictHashMap(..)
  , TrieMap
  , module Data.ListTrie.Map
  )
where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.ListTrie.Map          hiding (TrieMap)
import qualified Data.ListTrie.Map          (TrieMap)
import           Data.ListTrie.Base.Map
import           Data.Hashable
import qualified Data.HashMap.Strict
import           Data.Semigroup
import           Data.Traversable
import           Data.Tuple

newtype StrictHashMap k v = StrictHashMap { strictHashMap :: Data.HashMap.Strict.HashMap k v } deriving (Eq, Show, Read)

type TrieMap k v = Data.ListTrie.Map.TrieMap StrictHashMap k v

instance (Eq k, Hashable k) => Functor (StrictHashMap k) where
  fmap f (StrictHashMap {..}) = StrictHashMap $ f <$> strictHashMap

instance (Eq k, Hashable k) => Traversable (StrictHashMap k) where
  traverse f (StrictHashMap {..}) = StrictHashMap <$> f `traverse` strictHashMap

instance (Eq k, Hashable k) => Foldable (StrictHashMap k) where
  foldr f x = Data.HashMap.Strict.foldr f x . strictHashMap

instance (Eq k, Hashable k) => Map StrictHashMap k where
  eqCmp = const (==)
  null = Data.HashMap.Strict.null . strictHashMap
  lookup key = Data.HashMap.Strict.lookup key . strictHashMap
  alter f k = StrictHashMap . Data.HashMap.Strict.alter f k . strictHashMap
  unionWithKey f (StrictHashMap map1) (StrictHashMap map2) = StrictHashMap $ Data.HashMap.Strict.unionWithKey f map1 map2
  intersectionWithKey f (StrictHashMap map1) (StrictHashMap map2) = StrictHashMap $ Data.HashMap.Strict.intersectionWithKey f map1 map2
  differenceWithKey f (StrictHashMap map1) (StrictHashMap map2) = StrictHashMap $ strictHashMapDifferenceWithKey f map1 map2
  toListKV = Data.HashMap.Strict.toList . strictHashMap
  fromListKV = StrictHashMap . Data.HashMap.Strict.fromList
  empty = StrictHashMap $ Data.HashMap.Strict.empty
  isSubmapOfBy f (StrictHashMap map1) (StrictHashMap map2) = strictHashMapIsSubmapOfBy f map1 map2 
  mapAccumWithKey f x (StrictHashMap map1) = second StrictHashMap $ strictHashMapMapAccumWithKey f x map1
  filter f = StrictHashMap . Data.HashMap.Strict.filter f . strictHashMap

strictHashMapDifferenceWithKey :: (Eq k, Hashable k) => (k -> a -> b -> Maybe a) -> Data.HashMap.Strict.HashMap k a -> Data.HashMap.Strict.HashMap k b -> Data.HashMap.Strict.HashMap k a
strictHashMapDifferenceWithKey f map1 map2 =
  let
    sameKeyResults = Data.HashMap.Strict.intersectionWithKey f map1 map2
    keysToInclude = Data.HashMap.Strict.mapMaybe id sameKeyResults -- All the 'Just' keys with the Maybe stripped off
  in
      Data.HashMap.Strict.union keysToInclude            -- And put the 'Just'-result keys back in
    $ Data.HashMap.Strict.difference map1 sameKeyResults -- take duplicate keys out

strictHashMapIsSubmapOfBy :: (Eq k, Hashable k) => (a -> b -> Bool) -> Data.HashMap.Strict.HashMap k a -> Data.HashMap.Strict.HashMap k b -> Bool
strictHashMapIsSubmapOfBy f map1 map2 =
     (Data.HashMap.Strict.null $ Data.HashMap.Strict.difference map1 map2) -- All from map1 are present in map2
  && (Data.HashMap.Strict.foldr (&&) True $ Data.HashMap.Strict.intersectionWith f map1 map2) -- Test each parallel key for equality

strictHashMapMapAccumWithKey :: (Eq k, Hashable k) => (a -> k -> b -> (a, c)) -> a -> Data.HashMap.Strict.HashMap k b -> (a, Data.HashMap.Strict.HashMap k c)
strictHashMapMapAccumWithKey f x map =
    swap
  $ flip runState x
  $ Data.HashMap.Strict.traverseWithKey
    (\k v -> do
      x <- get   
      let (x', v') = f x k v
      put x'
      return v')
    map
 
{-
newtype LazyHashMap k v = LazyHashMap { lazyHashMap :: Data.HashMap.Lazy.HashMap k v } deriving (Eq, Show, Read)

instance (Hashable k) => Functor (LazyHashMap k) where
  fmap f (LazyHashMap {..}) = LazyHashMap $ f <$> lazyHashMap

instance (Hashable k) => Traversable (LazyHashMap k)  where
  traverse f (LazyHashMap {..}) = LazyHashMap <$> f `traverse` lazyHashMap

instance (Hashable k) => Foldable (LazyHashMap k) where
  foldr f x = foldr f x . lazyHashMap
-}
