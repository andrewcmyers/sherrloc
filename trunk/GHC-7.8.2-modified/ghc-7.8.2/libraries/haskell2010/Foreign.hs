{-# LANGUAGE PackageImports #-}

module Foreign (
        -- | The module @Foreign@ combines the interfaces of all
        -- modules providing language-independent marshalling support,
        -- namely
        module Data.Bits
        , module Data.Int
        , module Data.Word
        , module Foreign.Ptr
        , module Foreign.ForeignPtr
        , module Foreign.StablePtr
        , module Foreign.Storable
        , module Foreign.Marshal
  ) where

import "this" Data.Bits
import "this" Data.Int
import "this" Data.Word
import "this" Foreign.Ptr
import "this" Foreign.ForeignPtr
import "this" Foreign.StablePtr
import "this" Foreign.Storable
import "this" Foreign.Marshal
