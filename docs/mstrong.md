# MStrong

The Monoid requirement may seem arbitrary, but it's less arbitrary than it seems!

In fact, the class hierarchy should likely be split even further:

```
class (MonoidStrong p, ComonoidStrong p) => Strong p where
  first :: p (a, c) (b, c)

class ComonoidStrong p where
  cFirst :: Comonoid m => p (a, m) (b, m)

class MonoidStrong p where
  mFirst :: Monoid m => p (a, m) (b, m)
```

Types like `Star []` can only only be strong if granted a Comonoid (which is implicit in haskell; every type has one) due to the need to "copy" the value into multiple outputs.
Types like `Costar []` can only only be strong if granted a Monoid.
