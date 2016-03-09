{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representations for specific types
--
-- The reason for using symbol names ending with @_t@ is that 'deriveRender'
-- uses everything that comes before @_@ when rendering the constructor.

module Data.TypeRep.Types.Basic where



import qualified Data.Typeable as Typeable

import Language.Syntactic
import Data.Constraint

import Data.TypeRep.Representation


data BoolType   a where Bool_t   :: BoolType   (Full Bool)
data CharType   a where Char_t   :: CharType   (Full Char)
data IntType    a where Int_t    :: IntType    (Full Int)
data FloatType  a where Float_t  :: FloatType  (Full Float)
data DoubleType a where Double_t :: DoubleType (Full Double)
data ListType   a where List_t   :: ListType (a :-> Full [a])
data FunType    a where Fun_t    :: FunType  (a :-> b :-> Full (a -> b))

boolType :: (Syntactic a, BoolType :<: Domain a, Internal a ~ Bool) => a
boolType = sugarSym Bool_t

charType :: (Syntactic a, CharType :<: Domain a, Internal a ~ Char) => a
charType = sugarSym Char_t

intType :: (Syntactic a, IntType :<: Domain a, Internal a ~ Int) => a
intType = sugarSym Int_t

floatType :: (Syntactic a, FloatType :<: Domain a, Internal a ~ Float) => a
floatType = sugarSym Float_t

doubleType :: (Syntactic a, DoubleType :<: Domain a, Internal a ~ Double) => a
doubleType = sugarSym Double_t

listType
    :: ( Syntactic list
       , Syntactic elem
       , Domain list ~ Domain elem
       , ListType :<: Domain list
       , Internal list ~ [Internal elem]
       , elem ~ c e
       , list ~ c l
           -- These last equalities are used to help type inference by forcing the representations
           -- to use the same type constructor (e.g. 'TR' or 'TypeRep')
       )
    => elem -> list
listType = sugarSym List_t

funType
    :: ( Syntactic fun
       , Syntactic a
       , Syntactic b
       , Domain fun ~ Domain a
       , Domain fun ~ Domain b
       , FunType :<: Domain fun
       , Internal fun ~ (Internal a -> Internal b)
       , a   ~ c x
       , b   ~ c y
       , fun ~ c z
       )
    => a -> b -> fun
funType = sugarSym Fun_t

instance Render ListType
  where
    renderSym List_t = "[]"
    renderArgs [a] List_t = "[" ++ a ++ "]"

instance Render FunType
  where
    renderSym Fun_t = "(->)"
    renderArgs = renderArgsSmart

instance Render BoolType where
  renderSym Bool_t = "Bool"
instance Render CharType where
  renderSym Char_t = "Char"
instance Render IntType where
  renderSym Int_t = "Int"
instance Render FloatType where
  renderSym Float_t = "Float"
instance Render DoubleType where
  renderSym Double_t = "Double"

instance TypeEq BoolType t where
  typeEqSym (Bool_t, Nil) (Bool_t, Nil)
    = do { return Data.Constraint.Dict }
instance TypeEq CharType t where
  typeEqSym (Char_t, Nil) (Char_t, Nil)
    = do { return Data.Constraint.Dict }
instance TypeEq IntType t where
  typeEqSym (Int_t, Nil) (Int_t, Nil)
    = do { return Data.Constraint.Dict }
instance TypeEq FloatType t where
  typeEqSym (Float_t, Nil) (Float_t, Nil)
    = do { return Data.Constraint.Dict }
instance TypeEq DoubleType t where
  typeEqSym (Double_t, Nil) (Double_t, Nil)
    = do { return Data.Constraint.Dict }
instance TypeEq t t => TypeEq ListType t where
  typeEqSym (List_t, (a :* Nil)) (List_t, (b :* Nil))
    = do { Data.Constraint.Dict <- typeEqM (TypeRep a) (TypeRep b);
           return Data.Constraint.Dict }
instance TypeEq t t => TypeEq FunType t where
  typeEqSym (Fun_t, (a :* (b :* Nil))) (Fun_t, (c :* (d :* Nil)))
    = do { Data.Constraint.Dict <- typeEqM (TypeRep a) (TypeRep c);
           Data.Constraint.Dict <- typeEqM (TypeRep b) (TypeRep d);
           return Data.Constraint.Dict }

instance Witness Any BoolType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any CharType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any IntType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any FloatType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any DoubleType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any ListType t where
  witSym _ _ = Data.Constraint.Dict
instance Witness Any FunType t where
  witSym _ _ = Data.Constraint.Dict

instance PWitness Any BoolType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any CharType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any IntType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any FloatType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any DoubleType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any ListType t where
  pwitSym _ _ = return Data.Constraint.Dict
instance PWitness Any FunType t where
  pwitSym _ _ = return Data.Constraint.Dict

instance Witness Typeable.Typeable BoolType t where
  witSym Bool_t Nil = Data.Constraint.Dict
instance Witness Typeable.Typeable CharType t where
  witSym Char_t Nil = Data.Constraint.Dict
instance Witness Typeable.Typeable IntType t where
  witSym Int_t Nil = Data.Constraint.Dict
instance Witness Typeable.Typeable FloatType t where
  witSym Float_t Nil = Data.Constraint.Dict
instance Witness Typeable.Typeable DoubleType t where
  witSym Double_t Nil = Data.Constraint.Dict
instance Witness Typeable.Typeable t t =>
         Witness Typeable.Typeable ListType t where
  witSym List_t (a :* Nil)
    = case
          wit
            (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable) (TypeRep a)
      of {
        Data.Constraint.Dict -> Data.Constraint.Dict }
instance Witness Typeable.Typeable t t =>
         Witness Typeable.Typeable FunType t where
  witSym Fun_t (a :* (b :* Nil))
    = case
          wit
            (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable) (TypeRep a)
      of {
        Data.Constraint.Dict
          -> case
                 wit
                   (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable) (TypeRep b)
             of {
               Data.Constraint.Dict -> Data.Constraint.Dict } }

instance PWitness Typeable.Typeable BoolType t where
  pwitSym Bool_t Nil = do { return Data.Constraint.Dict }
instance PWitness Typeable.Typeable CharType t where
  pwitSym Char_t Nil = do { return Data.Constraint.Dict }
instance PWitness Typeable.Typeable IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }
instance PWitness Typeable.Typeable FloatType t where
  pwitSym Float_t Nil = do { return Data.Constraint.Dict }
instance PWitness Typeable.Typeable DoubleType t where
  pwitSym Double_t Nil = do { return Data.Constraint.Dict }
instance PWitness Typeable.Typeable t t =>
         PWitness Typeable.Typeable ListType t where
  pwitSym List_t (a :* Nil)
    = do { Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable)
                                     (TypeRep a);
           return Data.Constraint.Dict }
instance PWitness Typeable.Typeable t t =>
         PWitness Typeable.Typeable FunType t where
  pwitSym Fun_t (a :* (b :* Nil))
    = do { Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable)
                                     (TypeRep a);
           Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Typeable.Typeable)
                                     (TypeRep b);
           return Data.Constraint.Dict }

instance Witness Eq BoolType t where
  witSym Bool_t Nil = Data.Constraint.Dict
instance Witness Eq CharType t where
  witSym Char_t Nil = Data.Constraint.Dict
instance Witness Eq IntType t where
  witSym Int_t Nil = Data.Constraint.Dict
instance Witness Eq FloatType t where
  witSym Float_t Nil = Data.Constraint.Dict
instance Witness Eq DoubleType t where
  witSym Double_t Nil = Data.Constraint.Dict
instance Witness Eq t t => Witness Eq ListType t where
  witSym List_t (a :* Nil)
    = case wit (Typeable.Proxy :: Typeable.Proxy Eq) (TypeRep a) of {
        Data.Constraint.Dict -> Data.Constraint.Dict }

instance PWitness Eq BoolType t where
  pwitSym Bool_t Nil = do { return Data.Constraint.Dict }
instance PWitness Eq CharType t where
  pwitSym Char_t Nil = do { return Data.Constraint.Dict }
instance PWitness Eq IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }
instance PWitness Eq FloatType t where
  pwitSym Float_t Nil = do { return Data.Constraint.Dict }
instance PWitness Eq DoubleType t where
  pwitSym Double_t Nil = do { return Data.Constraint.Dict }
instance PWitness Eq t t => PWitness Eq ListType t where
  pwitSym List_t (a :* Nil)
    = do { Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Eq) (TypeRep a);
           return Data.Constraint.Dict }

instance Witness Ord BoolType t where
  witSym Bool_t Nil = Data.Constraint.Dict
instance Witness Ord CharType t where
  witSym Char_t Nil = Data.Constraint.Dict
instance Witness Ord IntType t where
  witSym Int_t Nil = Data.Constraint.Dict
instance Witness Ord FloatType t where
  witSym Float_t Nil = Data.Constraint.Dict
instance Witness Ord DoubleType t where
  witSym Double_t Nil = Data.Constraint.Dict
instance Witness Ord t t => Witness Ord ListType t where
  witSym List_t (a :* Nil)
    = case wit (Typeable.Proxy :: Typeable.Proxy Ord) (TypeRep a) of {
        Data.Constraint.Dict -> Data.Constraint.Dict }

instance PWitness Ord BoolType t where
  pwitSym Bool_t Nil = do { return Data.Constraint.Dict }
instance PWitness Ord CharType t where
  pwitSym Char_t Nil = do { return Data.Constraint.Dict }
instance PWitness Ord IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }
instance PWitness Ord FloatType t where
  pwitSym Float_t Nil = do { return Data.Constraint.Dict }
instance PWitness Ord DoubleType t where
  pwitSym Double_t Nil = do { return Data.Constraint.Dict }
instance PWitness Ord t t => PWitness Ord ListType t where
  pwitSym List_t (a :* Nil)
    = do { Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Ord) (TypeRep a);
           return Data.Constraint.Dict }

instance Witness Show BoolType t where
  witSym Bool_t Nil = Data.Constraint.Dict
instance Witness Show CharType t where
  witSym Char_t Nil = Data.Constraint.Dict
instance Witness Show IntType t where
  witSym Int_t Nil = Data.Constraint.Dict
instance Witness Show FloatType t where
  witSym Float_t Nil = Data.Constraint.Dict
instance Witness Show DoubleType t where
  witSym Double_t Nil = Data.Constraint.Dict
instance Witness Show t t => Witness Show ListType t where
  witSym List_t (a :* Nil)
    = case wit (Typeable.Proxy :: Typeable.Proxy Show) (TypeRep a) of {
        Data.Constraint.Dict -> Data.Constraint.Dict }

instance PWitness Show BoolType t where
  pwitSym Bool_t Nil = do { return Data.Constraint.Dict }
instance PWitness Show CharType t where
  pwitSym Char_t Nil = do { return Data.Constraint.Dict }
instance PWitness Show IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }
instance PWitness Show FloatType t where
  pwitSym Float_t Nil = do { return Data.Constraint.Dict }
instance PWitness Show DoubleType t where
  pwitSym Double_t Nil = do { return Data.Constraint.Dict }
instance PWitness Show t t => PWitness Show ListType t where
  pwitSym List_t (a :* Nil)
    = do { Data.Constraint.Dict <- pwit
                                     (Typeable.Proxy :: Typeable.Proxy Show) (TypeRep a);
           return Data.Constraint.Dict }

instance Witness Num IntType t where
  witSym Int_t Nil = Data.Constraint.Dict
instance Witness Num FloatType t where
  witSym Float_t Nil = Data.Constraint.Dict
instance Witness Num DoubleType t where
  witSym Double_t Nil = Data.Constraint.Dict

instance PWitness Num IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }
instance PWitness Num FloatType t where
  pwitSym Float_t Nil = do { return Data.Constraint.Dict }
instance PWitness Num DoubleType t where
  pwitSym Double_t Nil = do { return Data.Constraint.Dict }

instance Witness Integral IntType t where
  witSym Int_t Nil = Data.Constraint.Dict

instance PWitness Integral IntType t where
  pwitSym Int_t Nil = do { return Data.Constraint.Dict }

-- 'PWitness' instances for non-members

instance PWitness Eq FunType t

instance PWitness Ord FunType t

instance PWitness Show FunType t

instance PWitness Num BoolType   t
instance PWitness Num CharType   t
instance PWitness Num ListType   t
instance PWitness Num FunType    t

instance PWitness Integral BoolType   t
instance PWitness Integral CharType   t
instance PWitness Integral FloatType  t
instance PWitness Integral DoubleType t
instance PWitness Integral ListType   t
instance PWitness Integral FunType    t
