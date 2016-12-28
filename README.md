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
          -- ^ No type specified, we need to infer
          | TyVar String
          -- ^ A type variable
          | TyCon String
          -- ^ A type constant (@Int@, @Set@, etc.)
          | TyApply Type Type
          -- ^ Type application, i.e. @Set Int@
          | Function Type Type
          -- ^ A function, i.e. @a -> b@
          | Forall String Type
          -- ^ Quantify a type variable in a type.

          -- ...Whatever other type constructs...
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
          | Forall String Type
          -- ...
```

...and redefine typechecking as `Term t -> m t`, and inference to `Term (Maybe t) -> m (Term t)`. Not only this but we can define typeclasses for both inference and typechecking:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
class Typecheckable term t m where

    typecheck :: term t -> m t

class Inferable term t m where

    infer :: term (Maybe t) -> m (term t)
```

### Higher-order types

Higher-order typesystems have a notion of "Types of types" or ["Kinds"](https://en.wikipedia.org/wiki/Kind_(type_theory)), implies the types themselves need to be typechecked - or "kindchecked" -, so we could redefine `Type` again as follows:

```haskell
data Type k = TyVar String
            | TyCon String
            | TyApply (Type k) (Type k)
            | Function (Type k) (Type k)
            | Forall (String, k) (Type k)
```

We focus on the new definition of `Forall` as this assigns a Type's Kind `k` at a type variable's quantification, essentially having the same purpose as `Term t`'s assigning of a type to a variable in a lambda abstraction for variables in the following term.

### Dependent types

[Dependent typesystems](https://en.wikipedia.org/wiki/Dependent_type) are a different story, as _everything_ is a type (including functions, strings, lambda abstractions). So parameterizing the types involves parameterizing the term itself as it's own typesystem.

One option is to hack this together with the `Const` datatype:

```haskell

data DepTerm = -- ... a definition of a dependently typed language ...

data Const a b = Const a

instance (MonadWhatever m) => Typecheckable (Const DepTerm) DepTerm m where
    -- ...  
```

but this doesn't give us any more type safety or assertions of the properties of the syntax trees.

#### Not Actually Tested Yet

One way we can get around this and maintain our gained properties is to use the `Fix` (fixed-point) functor:

```haskell
newtype Fix f = Fix { unfix :: (f (Fix f)) }
```

This takes a functor and applies it to itself recursively, turning a type of `* -> *` into a type `*`. And now we can write our instance as:

```haskell
data Leaf = Var String | Con String

data DepTerm t = LeafTerm Leaf
               | Apply (DepTerm t) (DepTerm t)
               | Lambda (String, t) (Depterm t)
               | Pi (String, t) t
               | Sigma (String t) t

instance (MonadWhatever m) => Typecheckable Depterm (Fix DepTerm) m where
    -- ...
```

We need to typecheck the dependent terms themselves, and we can

<!--
    Probably something about the fixed point functor? I'm not quite certain how
    to represent this yet.

    One idea is to have the typesystem for a dependent language parameterize the universe as well, and to recurse until the final used universe along the lines ofP

    instance Typecheckable (DepLang n) (Fix (DepLang (Succ n))) where
        ...

    but that doesn't really work?
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

## `Typecheckable` for language components

We can expand the use of these typeclasses beyond pure ASTs and associated typesystems to other language components such as type & typeclass declarations in haskell/ML-style languages, building up to whole module representations being instances of `Typecheckable`.

```haskell
```

***

[1] where a monad/applicative functor/etc. `m` provides any extra information, logging, error reporting and whatever else is needed to complete typechecking.
