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

-- | Pure typechecking class, makes explicit how pure typechecking computations can
-- be structured, here mostly for illustrative purposes.
class (Semigroup (TypeError term t), Functor term) => PureTypecheckable term t where

    -- | The representation of type errors, i.e. @[`String`]@ or another datatype
    -- that records the context of the type error.
    type TypeError term t :: *

    -- | The typing context, a state that can be refered to during typechecking
    -- and updated if new information is gained from the expression.
    type TypingContext term t :: *

    {-|
        Typecheck a term with an explicit typing context and error
        reporting structures, returning either type errors or the type
        of the term and an updated typing context.
    -}
    typecheckP :: term t
               -> TypingContext term t
               -> Either (TypeError term t) (t, TypingContext term t)

class (Semigroup (InferError term t), Functor term) => PureInferable term t where

    -- | Representation of errors that occur during inference, analogous to
    -- `TypeError` in it's function.
    type InferError term t :: *

    -- | The context needed to infer the types of a term, analogous to `TypingContext`
    -- in it's function.
    type InferContext term t :: *

    {-|
        Infer a term's types within the pure typechecking environment
        specified for `PureTypecheckable`.

        ===Behaviour
            * Inherits all laws of `Inferable` with respect to the term whose types are inferred.
    -}
    inferP :: term (Maybe t)
           -> InferContext term t
           -> Either (InferError term t) (term t, InferContext term t)

-- | A wrapper for embedding purely-typecheckable/inferable terms
-- in potenentially non-pure typechecking environments
newtype TypecheckPure term t =
    TypecheckPure {
        unwrapTypecheck :: term t
    } deriving (Eq, Ord, Show, Functor)

instance (  PureTypecheckable term t
         ,  MonadError (TypeError term t) m
         ,  MonadState (TypingContext term t) m
         )
         => Typecheckable (TypecheckPure term) t m where

    -- | All pure typechecking actions can be embedded in a monadic typechecker
    -- which has corresponding error and state instances.
    typecheck term =
        gets (typecheckP (unwrapTypecheck term)) >>= \case
            Right (ty, tyctx) -> do
                put tyctx
                return ty
            Left err -> throwError err

instance (  PureInferable term t
         ,  MonadError (InferError term t) m
         ,  MonadState (InferContext term t) m
         )
         => Inferable (TypecheckPure term) t m where

    infer term =
        gets (inferP (unwrapTypecheck term)) >>= \case
            Right (tyterm, inferctx) -> do
                put inferctx
                return (TypecheckPure tyterm)
            Left err -> throwError err
