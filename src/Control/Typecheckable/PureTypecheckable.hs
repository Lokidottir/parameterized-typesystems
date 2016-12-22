{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Typecheckable.PureTypecheckable where

import           Control.Typecheckable
import           Control.Monad.State.Class
import           Control.Monad.Except
import           Data.Semigroup
import           Data.Either.Validation

newtype TypecheckPure term t =
    TypecheckPure {
        unwrapTypecheck :: term t
    } deriving (Eq, Ord, Show, Functor)

-- | Pure typechecking class, makes explicit how pure typechecking computations
-- are usually structured.
class (Semigroup (TypeError term t), Functor term) => PureTypecheckable term t where

    -- | The associated type of typeerrors.
    type TypeError term t :: *

    -- | The typing context, a state that can be refered to
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
         => Typecheckable (TypecheckPure term) t m where

    typecheck term =
        gets (typecheckP (unwrapTypecheck term)) >>= \case
            Success (ty, tyctx) -> do
                put tyctx
                return ty
            Failure err -> throwError err

class (PureTypecheckable term t, Functor term) => PureInferable term t where

    inferP :: term (Maybe t)
           -> TypingContext term t
           -> Validation (TypeError term t) (term t, TypingContext term t)

instance (  PureInferable term t
         ,  MonadError (TypeError term t) m
         ,  MonadState (TypingContext term t) m
         )
         => Inferable (TypecheckPure term) t m where

    infer term =
        gets (inferP (unwrapTypecheck term)) >>= \case
            Success (tyterm, tyctx) -> do
                put tyctx
                return (TypecheckPure tyterm)
            Failure err -> throwError err
