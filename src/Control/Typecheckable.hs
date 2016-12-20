{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module containing typechecking functions and data structures.
-}
module Control.Typecheckable (
    -- * Typeclasses for Typechecking and Inference
      Typecheckable(..)
    , Inferable(..)
    , PureTypecheckable(..)
    , PureInferable(..)
) where

import           Control.Monad.State.Class
import           Control.Monad.State (StateT, State)
import           Control.Monad.Except
import           Data.Semigroup
import           Control.Lens
import           Data.Either.Validation

{-|
    A typeclass for typechecking terms (@term@) with a typesystem (@t@).
-}
class Typecheckable (term :: * -> *) t m where

    typecheck :: term t -> m t

class Inferable (term :: * -> *) t m where

    infer :: term (Maybe t) -> m (term t)

-- | Pure typechecking class for illustrating
class (Semigroup (TypeError term t)) => PureTypecheckable term t where

    type TypeError term t :: *

    type TypingContext term t :: *

    typecheckP :: term t
               -> TypingContext term t
               -> Validation (TypeError term t) (t, TypingContext term t)

-- | All pure typechecking actions can be embedded in a monadic typechecker
-- which has corresponding error and state instances.
instance (  PureTypecheckable term t
         ,  MonadError (TypeError term t) m
         ,  MonadState (TypingContext term t) m
         )
         => Typecheckable term t m where

    typecheck term =
        gets (typecheckP term) >>= \case
            Success (ty, tyctx) -> do
                put tyctx
                return ty
            Failure err -> throwError err

type TypecheckPure term t = ExceptT (TypeError term t) (State (TypingContext term t))

class (PureTypecheckable term t) => PureInferable term t where

    inferP :: term (Maybe t)
           -> TypingContext term t
           -> Validation (TypeError term t) (term t, TypingContext term t)

instance (  PureInferable term t
         ,  MonadError (TypeError term t) m
         ,  MonadState (TypingContext term t) m
         )
         => Inferable term t m where

    infer term =
        gets (inferP term) >>= \case
            Success (tyterm, tyctx) -> do
                put tyctx
                return tyterm
            Failure err -> throwError err

typecheckP' :: PureTypecheckable term t => term t -> TypecheckPure term t t
typecheckP' = typecheck

inferP' :: PureInferable term t => term (Maybe t) -> TypecheckPure term t (term t)
inferP' = infer
