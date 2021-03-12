{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Data.Profunctor.Cont where

-- Profunctor experiments on continuations

import Data.Profunctor
import Data.Profunctor.Arrow
import Data.Function
import qualified Control.Category as C
import Control.Category ((>>>))
import Data.Void

data ContP r a b =
    ContP {runContP :: a -> ((b -> r) -> r) }
    deriving Functor

instance C.Category (ContP r) where
 id = ContP (&)
 ContP bCrR . ContP aBrR = ContP $ \a cr ->
     aBrR a $ \b -> bCrR b cr

instance Profunctor (ContP r) where
  dimap l r (ContP f) = fmap r $ ContP (\a cr -> f (l a) cr)

instance ProfunctorApply (ContP r) where
  app = ContP \(ContP aBrR, a) br -> aBrR a br

class Profunctor p => ProfunctorCont p where
  -- callCC :: (p a b -> p x a) -> p x a
  -- callCC :: (p a x -> p a (Either x b)) -> p a b
  -- callCC :: (p (Either a a) x -> p a x) -> p a a
  -- callCC :: (p (Either b x) x -> p a b) -> p a b
  -- callCC :: (p b x -> p a b) -> p a b
  -- callCC :: p ((a -> p q b) -> p q a, q) a
  callCC :: (p a b -> p x a) -> p x a

instance Choice (ContP r) where
  right' (ContP f) = ContP $ \eCA eCBR ->
      case eCA of
          Left c -> eCBR (Left c)
          Right a -> f a (eCBR . Right)

instance Strong (ContP r) where
  first' (ContP aBrR) = ContP \(a, c) bcr -> aBrR a (bcr . (,c))

instance ProfunctorCont (ContP r) where
  callCC f = ContP \q ar ->
    let ContP x = f $ ContP \a _ -> ar a
     in x q ar

evalContP :: ContP r a r -> a -> r
evalContP (ContP f) a = f a id

reset :: ContP r a r -> ContP r' a r
reset  = arr . evalContP

shift :: ContP r (ContP r (a -> r) r) a
shift = ContP (evalContP)

neutralize :: ContP r r x
neutralize = ContP (\r _ -> r)

testP :: ContP String Int Int -- ContP String Int Int
testP = catcher >>> arr succ >>>  arr succ >>> arr succ
  where
    catcher :: ContP String Int Int
    catcher = dimap (\n -> if even n then Right n else Left n) (either absurd id) (lmap show neutralize +++ C.id)

testP'' :: ContP String Int Int
testP'' = callCC \cc ->
    catcher cc >>> arr succ >>> arr succ >>> arr succ
  where
    catcher :: ContP String Int Int -> ContP String Int Int
    catcher p = dimap (splitPred even) unify (p +++ C.id)

splitPred :: (a -> Bool) -> a -> Either a a
splitPred predicate a = (if predicate a then Right a else Left a)

unify :: Either a a -> a
unify = either id id


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

