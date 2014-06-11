{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Foreign.Marshal.Utils (
  -- * General marshalling utilities

  -- ** Combined allocation and marshalling
  --
  with,          -- :: Storable a => a -> (Ptr a -> IO b) -> IO b
  new,           -- :: Storable a => a -> IO (Ptr a)

  -- ** Marshalling of Boolean values (non-zero corresponds to 'True')
  --
  fromBool,      -- :: Num a => Bool -> a
  toBool,        -- :: Num a => a -> Bool

  -- ** Marshalling of Maybe values
  --
  maybeNew,      -- :: (      a -> IO (Ptr a))
                 -- -> (Maybe a -> IO (Ptr a))
  maybeWith,     -- :: (      a -> (Ptr b -> IO c) -> IO c)
                 -- -> (Maybe a -> (Ptr b -> IO c) -> IO c)
  maybePeek,     -- :: (Ptr a -> IO        b )
                 -- -> (Ptr a -> IO (Maybe b))

  -- ** Marshalling lists of storable objects
  --
  withMany,      -- :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res

  -- ** Haskellish interface to memcpy and memmove
  -- | (argument order: destination, source)
  --
  copyBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()
  moveBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()

  ) where
import "base" Foreign.Marshal.Utils
