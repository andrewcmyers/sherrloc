{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module System.Environment (
      getArgs,       -- :: IO [String]
      getProgName,   -- :: IO String
      getEnv,        -- :: String -> IO String
  ) where
import "base" System.Environment
