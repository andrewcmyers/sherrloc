{- --------------------------------------------------------------------------
// Macros to help make Typeable instances.
//
// INSTANCE_TYPEABLEn(tc,tcname,"tc") defines
//
//	instance Typeable/n/ tc
//	instance Typeable a => Typeable/n-1/ (tc a)
//	instance (Typeable a, Typeable b) => Typeable/n-2/ (tc a b)
//	...
//	instance (Typeable a1, ..., Typeable an) => Typeable (tc a1 ... an)
// --------------------------------------------------------------------------
-}

#ifndef TYPEABLE_H
#define TYPEABLE_H

--  // For GHC, we can use DeriveDataTypeable + StandaloneDeriving to
--  // generate the instances.

#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable1 tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable2 tycon
#define INSTANCE_TYPEABLE3(tycon,tcname,str) deriving instance Typeable3 tycon
#define INSTANCE_TYPEABLE4(tycon,tcname,str) deriving instance Typeable4 tycon
#define INSTANCE_TYPEABLE5(tycon,tcname,str) deriving instance Typeable5 tycon
#define INSTANCE_TYPEABLE6(tycon,tcname,str) deriving instance Typeable6 tycon
#define INSTANCE_TYPEABLE7(tycon,tcname,str) deriving instance Typeable7 tycon

#endif
