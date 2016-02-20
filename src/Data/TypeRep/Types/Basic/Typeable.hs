{-# LANGUAGE CPP #-}
#ifndef DISABLE_TH
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE UndecidableInstances #-}

-- | 'Typeable' instances for various types. The reason for having these in a
-- separate module is that it might be desired to have these instances with
-- other type representations.
--
-- For example, instead of the instance
--
-- > (BoolType :<: t) => Typeable t Bool
--
-- one might want to have
--
-- > Typeable MyTypeRep Bool

module Data.TypeRep.Types.Basic.Typeable where



import Language.Syntactic

import Data.TypeRep.Representation
#ifndef DISABLE_TH
import Data.TypeRep.TH
#endif
import Data.TypeRep.Types.Basic



instance (BoolType   :<: t) => Typeable t Bool   where typeRep' = boolType
instance (CharType   :<: t) => Typeable t Char   where typeRep' = charType
instance (IntType    :<: t) => Typeable t Int    where typeRep' = intType
instance (FloatType  :<: t) => Typeable t Float  where typeRep' = floatType
instance (DoubleType :<: t) => Typeable t Double where typeRep' = doubleType

instance (ListType :<: t, Typeable t a)               => Typeable t [a]      where typeRep' = listType typeRep'
instance (FunType  :<: t, Typeable t a, Typeable t b) => Typeable t (a -> b) where typeRep' = funType typeRep' typeRep'

#ifndef DISABLE_TH
deriveWitnessTypeable ''BoolType
deriveWitnessTypeable ''CharType
deriveWitnessTypeable ''IntType
deriveWitnessTypeable ''FloatType
deriveWitnessTypeable ''DoubleType
deriveWitnessTypeable ''ListType
deriveWitnessTypeable ''FunType

derivePWitnessTypeable ''BoolType
derivePWitnessTypeable ''CharType
derivePWitnessTypeable ''IntType
derivePWitnessTypeable ''FloatType
derivePWitnessTypeable ''DoubleType
derivePWitnessTypeable ''ListType
derivePWitnessTypeable ''FunType
#endif
