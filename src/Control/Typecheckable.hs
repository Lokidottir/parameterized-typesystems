{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
    Module vaugely defining
-}
module Control.Typecheckable (
    -- * Typeclasses for Typechecking and Inference
      Typecheckable(..)
    , Inferable(..)
    , inferUntyped
    , inferenceLaw
) where

import Data.Functor

{-|
    A typeclass for typechecking terms (@term@) with a typesystem (@t@) within a
    monad\/action\/applicative @m@ that is used to access information needed to
    typecheck the term i.e. a state & error monad or a database accessing monad or
    if all the knowledge is available always it might be the identity monad.
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
                (infer term >>= \\termInfered -> return (term $> () == termInfered $> ())) == (infer term >> return True)
                @
    -}
    infer :: term (Maybe t) -> m (term t)

-- | Given a completely untyped term, infer it's type.
inferUntyped :: Inferable term t m => term () -> m (term t)
inferUntyped term = infer (term $> Nothing)

-- | A function that asserts the inference law of structure.
inferenceLaw :: (Monad m, Eq (term ()), Inferable term t m) => term (Maybe t) -> m Bool
inferenceLaw term = do
    termInfered <- infer term
    return ((term $> ()) == (termInfered $> ()))
