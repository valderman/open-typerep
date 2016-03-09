{-# LANGUAGE CPP #-}
#ifndef DISABLE_TH
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE UndecidableInstances #-}

-- | Representations for signed and unsigned integer types
--
-- The reason for using symbol names ending with @_t@ is that 'deriveRender'
-- uses everything that comes before @_@ when rendering the constructor.

module Data.TypeRep.Types.IntWord where



import Data.Int
import qualified Data.Typeable as Typeable
import Data.Word
import Data.TypeRep.Representation
import Data.Constraint

import Language.Syntactic

#ifndef DISABLE_TH
import Data.TypeRep.TH
#endif



data IntWordType a
  where
    Int8_t   :: IntWordType (Full Int8)
    Int16_t  :: IntWordType (Full Int16)
    Int32_t  :: IntWordType (Full Int32)
    Int64_t  :: IntWordType (Full Int64)
    Word8_t  :: IntWordType (Full Word8)
    Word16_t :: IntWordType (Full Word16)
    Word32_t :: IntWordType (Full Word32)
    Word64_t :: IntWordType (Full Word64)

int8Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int8) => a
int8Type = sugarSym Int8_t

int16Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int16) => a
int16Type = sugarSym Int16_t

int32Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int32) => a
int32Type = sugarSym Int32_t

int64Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int64) => a
int64Type = sugarSym Int64_t

word8Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word8) => a
word8Type = sugarSym Word8_t

word16Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word16) => a
word16Type = sugarSym Word16_t

word32Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word32) => a
word32Type = sugarSym Word32_t

word64Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word64) => a
word64Type = sugarSym Word64_t

instance Render IntWordType where
  renderSym Int8_t = "Int8"
  renderSym Int16_t = "Int16"
  renderSym Int32_t = "Int32"
  renderSym Int64_t = "Int64"
  renderSym Word8_t = "Word8"
  renderSym Word16_t = "Word16"
  renderSym Word32_t = "Word32"
  renderSym Word64_t = "Word64"
instance TypeEq IntWordType t where
  typeEqSym (Int8_t, Nil) (Int8_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym (Int16_t, Nil) (Int16_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym (Int32_t, Nil) (Int32_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym (Int64_t, Nil) (Int64_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym (Word8_t, Nil) (Word8_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym
    (Word16_t, Nil)
    (Word16_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym
    (Word32_t, Nil)
    (Word32_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym
    (Word64_t, Nil)
    (Word64_t, Nil)
    = do { return Data.Constraint.Dict }
  typeEqSym _ _
    = error "what?"
instance Witness Any IntWordType t where
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
  witSym _ _ = Data.Constraint.Dict
instance PWitness Any IntWordType t where
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict
  pwitSym _ _
    = return Data.Constraint.Dict

instance Witness Typeable.Typeable IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict
instance Witness Eq IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict
instance Witness Ord IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict
instance Witness Show IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict
instance Witness Num IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict
instance Witness Integral IntWordType t where
  witSym Int8_t Nil
    = Data.Constraint.Dict
  witSym Int16_t Nil
    = Data.Constraint.Dict
  witSym Int32_t Nil
    = Data.Constraint.Dict
  witSym Int64_t Nil
    = Data.Constraint.Dict
  witSym Word8_t Nil
    = Data.Constraint.Dict
  witSym Word16_t Nil
    = Data.Constraint.Dict
  witSym Word32_t Nil
    = Data.Constraint.Dict
  witSym Word64_t Nil
    = Data.Constraint.Dict

instance PWitness Typeable.Typeable IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }

instance PWitness Eq IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }

instance PWitness Ord IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }

instance PWitness Show IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }

instance PWitness Num IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }

instance PWitness Integral IntWordType t where
  pwitSym Int8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Int64_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word8_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word16_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word32_t Nil
    = do { return Data.Constraint.Dict }
  pwitSym Word64_t Nil
    = do { return Data.Constraint.Dict }
