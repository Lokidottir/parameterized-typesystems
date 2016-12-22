{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
    Module vaugely defining
-}
module Control.Typecheckable (
    -- * Typeclasses for Typechecking and Inference
      Typecheckable(..)
    , Inferable(..)
) where
    
{-|
    A typeclass for typechecking terms (@term@) with a typesystem (@t@).
-}
class Typecheckable term t m where

    -- | Compute the type of a typed term.
    typecheck :: term t -> m t

class Inferable term t m where

    {-|
        Infer the types of term where the types of the values aren't entirely known.
    -}
    infer :: term (Maybe t) -> m (term t)
