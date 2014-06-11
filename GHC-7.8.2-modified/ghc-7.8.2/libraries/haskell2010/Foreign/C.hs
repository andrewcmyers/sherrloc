{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Foreign.C (

       -- | The module "Foreign.C" combines the interfaces of all
       -- modules providing C-specific marshalling support, namely

          module Foreign.C.Types
        , module Foreign.C.String
        , module Foreign.C.Error
  ) where

import "this" Foreign.C.Types
import "this" Foreign.C.String
import "this" Foreign.C.Error
