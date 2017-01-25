{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module vaugely defining
-}
module Control.Typecheckable (
    -- * Typeclasses for Typechecking and Inference
      Typecheckable(..)
    , Inferable(..)
    , Untyped
    , inferUntyped
    , inferenceLaw
) where

import Data.Functor

{-|
    A typeclass for typechecking terms (@term@) with a typesystem (@t@) within a
    monad\/action\/applicative @m@ that is used to access information needed to
    typecheck the term i.e. a state & error monad or a database accessing monad or
    if all the knowledge is available always it might be the identity monad.

    Other typecheckers might need to access external solvers, @m@ can do more or
    less anything as needed. It's not even enforced to be a monad.
-}
class (Functor term) => Typecheckable term t m where

    {-|
        Typecheck a term whose types are all known, and return the type of the term.
    -}
    typecheck :: term t -> m t

{-|
    A typecless for infering typed terms from partially-to-untyped terms.
-}
class Functor term => Inferable term t m where

    {-|
        Infer the types of term where the types of the values aren't entirely known.

        ===Behaviour

            *
              Law: Structure of the term must remain unchanged

                @
                -- Inference law when `m` is a monad.
                (infer term >>= \\termInfered -> return (term $> () == termInfered $> ())) == (infer term >> return True)
                @
    -}
    infer :: term (Maybe t) -> m (term t)

-- | Type alias for an untyped term, with it's parameterized
-- typesystem being set as the unit type.
type Untyped term = term ()

-- | Given a completely untyped term, infer it's type.
inferUntyped :: Inferable term t m => Untyped term -> m (term t)
inferUntyped term = infer (term $> Nothing)

-- | Assertion of `infer`'s structure-preserving law when `m` is a monad.
inferenceLaw :: (Monad m, Eq (term ()), Inferable term t m) => term (Maybe t) -> m Bool
inferenceLaw term = do
    termInfered <- infer term
    return ((term $> ()) == (termInfered $> ()))
