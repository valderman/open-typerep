-- | Utilities for polyvariadic functions

module Data.TypeRep.VarArg where



import Control.Monad.Error

import Data.Syntactic
import Data.TypeRep
import Data.TypeRep.Internal



----------------------------------------------------------------------------------------------------
-- * Working with polyvariadic functions
----------------------------------------------------------------------------------------------------

-- | Newtype marking the result of a N-ary function
newtype Res a = Res a

-- | Put a 'Res' marker at the result type of a function
--
-- > ToRes (a -> b -> ... -> x) = a -> b -> ... -> Res x
type family ToRes a where
  ToRes (a -> b) = a -> ToRes b
  ToRes a        = Res a

-- | Remove the 'Res' marker at the result type of a function
--
-- > FromRes (a -> b -> ... -> Res x) = a -> b -> ... -> x
type family FromRes a where
  FromRes (a -> b) = a -> FromRes b
  FromRes (Res a)  = a

-- | Witness of the arity of a function. 'Arity' will normally be indexed by @(`ToRes` a)@.
data Arity a
  where
    FunRes :: Arity (Res a)
    FunArg :: Arity b -> Arity (a -> b)

class VarArg t
  where
    aritySym :: VarArg u => t sig -> Args (AST u) sig -> Arity (ToRes (DenResult sig))
    fromResInvSym :: (VarArg u, a ~ DenResult sig) =>
        t sig -> Args (AST u) sig -> Dict (FromRes (ToRes a) ~ a)

instance (VarArg t1, VarArg t2) => VarArg (t1 :+: t2)
  where
    aritySym      (InjL t) = aritySym t
    aritySym      (InjR t) = aritySym t
    fromResInvSym (InjL t) = fromResInvSym t
    fromResInvSym (InjR t) = fromResInvSym t

instance VarArg IntType
  where
    aritySym IntType Nil      = FunRes
    fromResInvSym IntType Nil = Dict

instance VarArg FunType
  where
    aritySym FunType (_ :* b :* Nil) = FunArg $ arity $ TypeRep b
    fromResInvSym FunType (_ :* b :* Nil)
        | Dict <- fromResInv $ TypeRep b = Dict

-- | Get the 'Arity' of a type. The purpose is to be able to distinguish between functions and
-- non-functions without having to handle all cases of a 'TypeRep'.
arity :: VarArg t => TypeRep t a -> Arity (ToRes a)
arity = simpleMatch aritySym . unTypeRep

-- | Prove that 'FromRes' is the inverse of 'ToRes'
fromResInv :: VarArg t => TypeRep t a -> Dict (FromRes (ToRes a) ~ a)
fromResInv = simpleMatch fromResInvSym . unTypeRep

-- TODO With injective type families `fromResInv` is probably not going to be needed:
--
--   https://ghc.haskell.org/trac/ghc/ticket/6018

type NonFunction a = ToRes a ~ Res a

-- | Attempt to prove that a type is not a function type
nonFunction :: (VarArg t, MonadError String m) => TypeRep t a -> m (Dict (NonFunction a))
nonFunction t | Dict <- fromResInv t = case arity t of
    FunRes -> return Dict
    _      -> throwError "nonFunction: function type"



----------------------------------------------------------------------------------------------------
-- * N-ary monadic functions
----------------------------------------------------------------------------------------------------

-- | Give a function a monadic result type. @(`FunM` m)@ will normally be indexed by @(`ToRes` a)@.
--
-- > FunM m (a -> b -> ... -> Res x) = a -> b -> ... -> m x
type family FunM m a where
  FunM m (a -> b) = a -> FunM m b
  FunM m (Res a)  = m a

-- | Lift a function to a similar function with monadic result type
--
-- > liftMonadic f = \a b ... x -> return (f a b ... x)
liftMonadic :: forall t a m . (VarArg t, Monad m) => Proxy m -> TypeRep t a -> a -> FunM m (ToRes a)
liftMonadic _ t f | Dict <- fromResInv t = go (arity t) f
  where
    go :: (FromRes (ToRes b) ~ b) => Arity (ToRes b) -> b -> FunM m (ToRes b)
    go FunRes     a = return a
    go (FunArg b) f = \a -> go b (f a)

-- | Give a function monadic arguments and result type. @(`FunM2` m)@ will normally be indexed by
-- @(`ToRes` a)@.
--
-- > FunM m (a -> b -> ... -> Res x) = m a -> m b -> ... -> m x
type family FunM2 m a where
  FunM2 m (a -> b) = m a -> FunM2 m b
  FunM2 m (Res a)  = m a

-- | Lift a function to a similar function with monadic arguments and result
--
-- > liftMonadic f = \ma mb ... mx -> do
-- >     a <- ma
-- >     b <- mb
-- >     ...
-- >     x <- mx
-- >     return (f a b ... x)
liftMonadic2 :: forall t a m . (VarArg t, Monad m) =>
    Proxy m -> TypeRep t a -> a -> FunM2 m (ToRes a)
liftMonadic2 _ t f | Dict <- fromResInv t = go (arity t) (return f)
  where
    go :: (FromRes (ToRes b) ~ b) => Arity (ToRes b) -> m b -> FunM2 m (ToRes b)
    go FunRes     ma = ma
    go (FunArg b) mf = \ma -> go b $ do
        f <- mf
        a <- ma
        return (f a)
