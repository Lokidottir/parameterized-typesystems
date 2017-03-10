# Parameterized typesystems

A library exposing typeclasses for typechecking terms parametrized in their typesystem.

## Languages & their typesystems

Many typed languages written in haskell come in two parts (examples here may be vastly simplified):

```haskell
data Term = Var String
          -- ^ A variable
          | Con String
          -- ^ A term constant, i.e. @Nothing@ or @10@
          | Apply Term Term
          -- ^ Application of two terms i.e. @id f@
          | Lambda (String, Maybe Type) Term
          -- ^ A lambda abstraction, with a possibly-undefined type
          -- ...

data Type = TyVar String
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

...and redefine typechecking as `Term t -> m t`, and inference to `Term (Maybe t) -> m (Term t)` (This isn't an orthordox definition as far as I can tell, see [A note on inference](#a-note-on-inference). Not only this but we can define typeclasses for both inference and typechecking:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
class Typecheckable term t m where

    typecheck :: term t -> m t

class Inferable term t m where

    infer :: term (Maybe t) -> m (term t)
```

#### A note on inference.

We discriminate between two kinds of inference, one that gives us the type of a fully type-annotated term, and another that "fills in" untyped gaps in unannotated types of a term. We regard the former as reasonable to expect to able to be derived from a fully-annotated term while checking it's correctness and the latter as an operation seperate from typechecking.

i.e.

```
typechecking derives
λ (a : Int) (b : Int). a + b
has type
Int -> Int -> Int

while inference derives
λ a b. a + b
annotates as
λ (a : Int) (b : Int). a + b
```

### Higher-order types

Higher-order typesystems have a notion of "Types of types" or ["Kinds"](https://en.wikipedia.org/wiki/Kind_(type_theory)), which implies the types themselves need to be typechecked - or "kindchecked" -, so we could redefine `Type` again as follows:

```haskell
data Type k = TyVar String
            | TyCon String
            | TyApply (Type k) (Type k)
            | Function (Type k) (Type k)
            | Forall (String, k) (Type k)
```

We focus on the new definition of `Forall` as this assigns a Type's Kind `k` at a type variable's quantification, essentially having the same purpose as `Term t`'s assigning of a type to a variable in a lambda abstraction for variables in the following term.

We can also introduce another data structure for Kinds:

```haskell
data Kind = Star
          | KindFunction Kind Kind
          -- ...
```

And define a `Typecheckable` instance for `Type k`.

```haskell
instance (...) => Typecheckable Type Kind m where
    -- ...

instance (...) => Inferable Type Kind m where
    -- ...
```

### Dependent types

[Dependent typesystems](https://en.wikipedia.org/wiki/Dependent_type) are a different story, as _everything_ is a type (including functions, strings, lambda abstractions). So parameterizing the types involves parameterizing the term itself as it's own typesystem.

One option is to hack this together with the `Const` datatype:

```haskell
data DepTerm t = Var String
               | Con String
               | Apply DepTerm DepTerm
               | Lambda (String, DepTerm) DepTerm
               | Pi (String, DepTerm) DepTerm

data Const a b = Const a

instance (...) => Typecheckable (Const DepTerm) DepTerm m where
    -- ...  
```

but this doesn't give us any more type safety or assertions of the properties of the syntax trees.

One way we can get around this and maintain our gained properties is to use the `Fix` (fixed-point) functor:

```haskell
newtype Fix f = Fix { unfix :: f (Fix f) }
```

This takes a functor and applies it to itself recursively, turning a type of `* -> *` into a type `*`. And now we can write our instance as having a fixed point in it's typesystem:

```haskell
-- We say that dependently-typed terms are `Fixed-point` in their types.
data DepTerm t = Var String
               | Con String
               | Apply (DepTerm t) (DepTerm t)
               | Lambda (String, t) (DepTerm t)
               | Pi (String, t) t
               -- ^ We changed pi-types to show that they quantify over a type!

instance (...) => Typecheckable DepTerm (Fix DepTerm) m where
    -- ...
```

During typechecking we can unfurl `Fix DepTerm` to `DepTerm (Fix DepTerm)` and keep typechecking until we reach the end of the tree.

<!--
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
-->

***

[1] where a monad/applicative functor/etc. `m` provides any extra information, logging, error reporting and whatever else is needed to complete typechecking.
