# purescript-binary-tree

A binary search tree – an exercise implementing common type-classes in PureScript.

This project contains the following typeclass instances:

- Show
- Eq
- Functor
- Semigroup
- Monoid
- Apply
- Applicative
- Bind
- Monad
- Foldable
- Traversable

The following laws are satisfied and verified in the project unit tests

- Functor 
  - Identity: `map identity = identidty`
  - Composition: `map (f <<< g) = map f <<< map g`

- Apply 
  - Associative Composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`

- Applicative
  - Identity: `(pure identity) <*> v = v`
  - Composition: `pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)`
  - Homomorphism: `pure f <*> (pure x) = pure (f x)`
  - Interchange: `u <*> (pure y) = (pure (_ $ y)) <*> u`

- Bind 
  - Associativity: `(x >>= f) >>= g = x >>= (\k -> f k >>= g)`
  - Apply Superclass: `apply f x = f >>= \f' -> map f' x`

- Monad
  - Left Identity: `pure x >>= f = f x`
  - Right Identity: `x >>= pure = x`

