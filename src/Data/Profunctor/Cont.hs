{-# LANGUAGE LambdaCase #-}
module Data.Profunctor.Cont where

-- Profunctor experiments on continuations

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
-- import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.Class
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Data.Profunctor.Arrow
import Data.Function
import qualified Control.Category as C
import Control.Category ((>>>))
import Data.Void
-- ContT r m a :: (a -> m r) -> m r
-- shiftT :: ((a -> m r) -> ContT r m r) -> ContT r m a
-- shiftT :: ((a -> m r) -> (r -> m r) -> m r) -> (a -> m r) -> m r

import Data.Functor.Identity


data ContP r a b =
    ContP {runContP :: a -> ((b -> r) -> r) }
    deriving Functor

instance C.Category (ContP r) where
 id = ContP (&)
 ContP bCrR . ContP aBrR = ContP $ \a cr ->
     aBrR a $ \b -> bCrR b cr

instance Profunctor (ContP r) where
  dimap l r (ContP f) = fmap r $ ContP (\a cr -> f (l a) cr)

class Profunctor p => ProfunctorCont p where
  -- callCC :: (p a b -> p x a) -> p x a
  -- callCC :: (p a x -> p a (Either x b)) -> p a b
  -- callCC :: (p (Either a a) x -> p a x) -> p a a
  -- callCC :: (p (Either b x) x -> p a b) -> p a b
  -- callCC :: (p b x -> p a b) -> p a b
  callCC :: p ((a -> p q b) -> p q a, q) a

instance Choice (ContP r) where
  right' (ContP f) = ContP $ \eCA eCBR ->
      case eCA of
          Left c -> eCBR (Left c)
          Right a -> f a (eCBR . Right)

instance ProfunctorCont (ContP r) where
  callCC = callCC'

evalContP :: ContP r a r -> a -> r
evalContP (ContP f) a = f a id

neutralize :: ContP r r x
neutralize = ContP (\r _ -> r)

reset :: ContP r a r -> ContP r' a r
reset  = liftP . evalContP

shift :: ContP r (ContP r (a -> r) r) a
shift = ContP (evalContP)

callCC' :: ContP r ((a -> ContP r q b) -> ContP r q a, q) a
callCC' = ContP $ \(f,q) c ->
    let (ContP z) = f (\x -> ContP $ \_ _ -> c x)
     in z q c

liftP :: (a -> b) -> ContP r a b
liftP f = ContP (\a br -> br (f a))


testP :: ContP String Int Int -- ContP String Int Int
-- testP = callCC catcher >>> arr succ >>>  arr succ >>> arr succ
testP = catcher >>> arr succ >>>  arr succ >>> arr succ
  where
    -- catcher :: ContP String Int Void -> ContP String Int Int
    -- catcher p = dimap (\n -> if even n then Right n else Left n) (either absurd id) (p +++ C.id)
    catcher :: ContP String Int Int
    catcher = dimap (\n -> if even n then Right n else Left n) (either absurd id) (lmap show neutralize +++ C.id)

-- helper :: (a -> Bool) -> [a] -> ContT r f (Maybe a)
-- helper predicate xs = do
--     callCC $ \cc -> do
--         case find predicate xs of
--           Just i -> cc (Just i)
--           Nothing -> pure Nothing

-- helper' :: (Monad m, Monoid r) => (a -> Bool) -> [a] -> ContT r m a
-- helper' predicate xs = do
--     shiftT $ \cc -> do
--         getAp $ flip foldMap xs $ \x ->
--                     Ap $ if predicate x
--                             then lift (cc x)
--                             else pure mempty

-- helper'' :: (Monad m, Monoid r) => (r -> Bool) -> [r] -> ContT r m r
-- helper'' predicate xs = do
--     callCC $ \outer -> do
--         shiftT $ \inner -> do
--             foldl' (go inner outer) (pure mempty) xs
--             -- getAp $ flip foldMap xs $ \x ->
--             --             Ap $ if predicate x
--             --                     then outer _
--             --                     else lift $ inner x
--   where
--     go inner outer mr a
--       | predicate a = mr >>= outer
--       | otherwise = liftA2 (<>) mr (lift $ inner a)

-- stopWhen :: (Representable p, Rep p ~ f) => p (Maybe Int) r -> p [Int] r
-- stopWhen = withCapture (helper even)

-- stopWhen' :: (Monoid r, Monad m, Representable p, Rep p ~ m) => p Int r -> p [Int] r
-- stopWhen' = withCapture (helper' even)

-- stopWhen'' :: (Monad m, Representable p, Rep p ~ m) => p [a] [a] -> p [[a]] [a]
-- stopWhen'' = withCapture (helper'' ((>3) . length))


-- -- Optic s r a r =
-- withCapture :: (Representable p, Rep p ~ f) => (s -> ContT r f a) -> p a r -> p s r
-- withCapture f p =
--     tabulate $ \b ->
--         let ContT g = (f b)
--             handler = sieve p
--          in g handler


-- tester :: [[ Int ]] -> IO [Int]
-- tester = runStar $ stopWhen'' (Star go')
--   where
--     go' i = print i >> pure i
--     go (Just i) = print i >> pure [i]
--     go Nothing = pure []

-- -- class Profunctor p => Capture p where

