# Parameterized typesystems

## Languages & their typesystems

We can imagine many typed languages in two parts: their expressions/terms and their typesystems:

```haskell
data Term = Var String
          | Con String
          | Apply Term Term
          | Lambda (String, Type) Term
          -- ...

data Type = UndeclaredType
          | TyVar String
          | TyCon String
          | TyApply Type Type
          | Function Type Type
          -- ...
```

Operations such as typechecking and inference in this definition can be loosely defined as having the types `Term -> m Type` and `Term -> m Term`[1] respectively

But we can type this better, and open up a lot of opportunity if we pull out a specific type AST from `Term` and replace it with a type variable, yielding:

```haskell
data Term t = Var String
            | Con String
            | Apply (Term t) (Term t)
            | Lambda (String, t) (Term t)
            -- ...
```

Then simplify the `Type` datatype slightly:

```haskell
data Type = TyVar String
          | TyCon String
          | TyApply Type Type
          | Function Type Type
          -- ...
```

...and redefine typechecking as `Term t -> m t`, and inference to `Term (Maybe t) -> m (Term t)`. Not only this but we can define typeclasses for both inference and typechecking:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
class Typecheckable term t m where

    typecheck :: term t -> m t

class Inferable term t m where

    infer :: term (Maybe t) -> m t
```

### Dependent types

<!--
    Probably something about the fixed point functor? I'm not quite certain how
    to represent this yet.
-->

## Pure typechecking

Pure typechecking systems can be explicitly defined as having a method to report type errors and a typing context state. This can be defined explicitly in another typeclass:

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
class (Semigroup (TypeError term t)) => PureTypecheckable term t where

    type TypeError term t :: *

    type TypingContext term t :: *

    -- Using 'Validation' from the 'either' package as it's semigroup instance
    -- combines errors instead of only reporting the first one encountered
    -- as `Either`'s does.
    typecheckP :: term t -> TypingContext term t -> Validation (TypeError term t) (t, TypingContext term t)

class (PureTypecheckable term t) => PureInferable term t where

    inferP :: term (Maybe t) -> TypingContext term t -> Validation (TypeError term t) (term t, TypingContext term t)
```

***

[1] where a monad/applicative functor/etc. `m` provides any extra information, logging, error reporting and whatever else is needed to complete typechecking.
