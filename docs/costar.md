# Costar

Costar is good for signal processing and is kinda comonadic over representable structures!

E.g. you can linearly interpolate or take the gaussian blur of a signal using something like:

```haskell
blur :: Costar ((->) Int) Int Float
blur = cotabulate go
  where
    go :: (Int -> Int) -> Float
    go f = fromIntegral (f (-1) + f 0 + f 1) / 3
```

Costar is good for these sort of very linear processes. It can work along a comonadic context, but can't make decisions, and isn't strong (but is Monoidally strong)

If you have a Monoid it works like Traced, which incidentally is a profunctor I think.
