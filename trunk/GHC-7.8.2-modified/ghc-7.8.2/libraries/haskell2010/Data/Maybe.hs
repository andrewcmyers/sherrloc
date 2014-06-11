{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.Maybe (
   -- * The @Maybe@ type and operations

     Maybe(Nothing,Just)-- instance of: Eq, Ord, Show, Read,
                        --              Functor, Monad, MonadPlus

   , maybe              -- :: b -> (a -> b) -> Maybe a -> b

   , isJust             -- :: Maybe a -> Bool
   , isNothing          -- :: Maybe a -> Bool
   , fromJust           -- :: Maybe a -> a
   , fromMaybe          -- :: a -> Maybe a -> a
   , listToMaybe        -- :: [a] -> Maybe a
   , maybeToList        -- :: Maybe a -> [a]
   , catMaybes          -- :: [Maybe a] -> [a]
   , mapMaybe           -- :: (a -> Maybe b) -> [a] -> [b]

   -- * Specification

   -- $code

  ) where
import "base" Data.Maybe

{- $code
> module Data.Maybe(
>     Maybe(Nothing, Just),
>     isJust, isNothing,
>     fromJust, fromMaybe, listToMaybe, maybeToList,
>     catMaybes, mapMaybe,
>     maybe
>   ) where
>
> maybe                  :: b -> (a -> b) -> Maybe a -> b
> maybe n _ Nothing      =  n
> maybe _ f (Just x)     =  f x
>
> isJust                 :: Maybe a -> Bool
> isJust (Just a)        =  True
> isJust Nothing         =  False
>
> isNothing              :: Maybe a -> Bool
> isNothing              =  not . isJust
>
> fromJust               :: Maybe a -> a
> fromJust (Just a)      =  a
> fromJust Nothing       =  error "Maybe.fromJust: Nothing"
>
> fromMaybe              :: a -> Maybe a -> a
> fromMaybe d Nothing    =  d
> fromMaybe d (Just a)   =  a
>
> maybeToList            :: Maybe a -> [a]
> maybeToList Nothing    =  []
> maybeToList (Just a)   =  [a]
>
> listToMaybe            :: [a] -> Maybe a
> listToMaybe []         =  Nothing
> listToMaybe (a:_)      =  Just a
>
> catMaybes              :: [Maybe a] -> [a]
> catMaybes ms           =  [ m | Just m <- ms ]
>
> mapMaybe               :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe f             =  catMaybes . map f
-}
