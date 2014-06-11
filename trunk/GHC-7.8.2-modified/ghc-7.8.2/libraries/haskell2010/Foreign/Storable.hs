{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Foreign.Storable
        ( Storable(
             sizeOf,         -- :: a -> Int
             alignment,      -- :: a -> Int
             peekElemOff,    -- :: Ptr a -> Int      -> IO a
             pokeElemOff,    -- :: Ptr a -> Int -> a -> IO ()
             peekByteOff,    -- :: Ptr b -> Int      -> IO a
             pokeByteOff,    -- :: Ptr b -> Int -> a -> IO ()
             peek,           -- :: Ptr a             -> IO a
             poke)           -- :: Ptr a        -> a -> IO ()
  ) where
import "base" Foreign.Storable
