module Utils.Map where

import RIO
import qualified RIO.Map as Map

forMap :: (Ord k, Monad m) => Map k v -> (k -> v -> m a) -> m (Map k a)
forMap m f = Map.fromList <$> for (Map.assocs m) \(k, v) -> (k,) <$> f k v

unionWithM :: (Monad m, Ord k) => (v -> v -> m v) ->
  Map k v -> Map k v -> m (Map k v)
unionWithM f x y = sequence $ Map.unionWith
  (\a b -> join $ liftM2 f a b)
  (return <$> x)
  (return <$> y)

unionsWithM :: (Monad m, Foldable f, Ord k) => (v -> v -> m v) ->
  f (Map k v) -> m (Map k v)
unionsWithM f = foldl' (\x y -> x >>= unionWithM f y) $ return mempty

fromListM :: (Monad m, Foldable f, Functor f, Ord k) =>
  (v -> v -> m v) -> f (k, v) -> m (Map k v)
fromListM f = unionsWithM f . fmap (uncurry Map.singleton)
