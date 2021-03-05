{-# LANGUAGE LambdaCase #-}
module Data.Profunctor.Cont where

-- Profunctor experiments on continuations

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative

-- ContT r m a :: (a -> m r) -> m r
-- shiftT :: ((a -> m r) -> ContT r m r) -> ContT r m a
-- shiftT :: ((a -> m r) -> (r -> m r) -> m r) -> (a -> m r) -> m r

import Data.Functor.Identity


helper :: (a -> Bool) -> [a] -> ContT r f (Maybe a)
helper predicate xs = do
    callCC $ \cc -> do
        case find predicate xs of
          Just i -> cc (Just i)
          Nothing -> pure Nothing

helper' :: (Monad m, Monoid r) => (a -> Bool) -> [a] -> ContT r m a
helper' predicate xs = do
    shiftT $ \cc -> do
        getAp $ flip foldMap xs $ \x ->
                    Ap $ if predicate x
                            then lift (cc x)
                            else pure mempty

helper'' :: (Monad m, Monoid r) => (r -> Bool) -> [r] -> ContT r m r
helper'' predicate xs = do
    callCC $ \outer -> do
        shiftT $ \inner -> do
            foldl' (go inner outer) (pure mempty) xs
            -- getAp $ flip foldMap xs $ \x ->
            --             Ap $ if predicate x
            --                     then outer _
            --                     else lift $ inner x
  where
    go inner outer mr a 
      | predicate a = mr >>= outer
      | otherwise = liftA2 (<>) mr (lift $ inner a)

stopWhen :: (Representable p, Rep p ~ f) => p (Maybe Int) r -> p [Int] r
stopWhen = withCapture (helper even)

stopWhen' :: (Monoid r, Monad m, Representable p, Rep p ~ m) => p Int r -> p [Int] r
stopWhen' = withCapture (helper' even)

stopWhen'' :: (Monad m, Representable p, Rep p ~ m) => p [a] [a] -> p [[a]] [a]
stopWhen'' = withCapture (helper'' ((>3) . length))


-- Optic s r a r =
withCapture :: (Representable p, Rep p ~ f) => (s -> ContT r f a) -> p a r -> p s r
withCapture f p =
    tabulate $ \b ->
        let ContT g = (f b)
            handler = sieve p
         in g handler


tester :: [[ Int ]] -> IO [Int]
tester = runStar $ stopWhen'' (Star go')
  where
    go' i = print i >> pure i
    go (Just i) = print i >> pure [i]
    go Nothing = pure []

-- class Profunctor p => Capture p where

