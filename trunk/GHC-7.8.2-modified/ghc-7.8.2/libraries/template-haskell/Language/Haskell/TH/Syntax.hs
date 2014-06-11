{-# LANGUAGE DeriveDataTypeable, MagicHash, PolymorphicComponents, RoleAnnotations, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.TH.Syntax where

import GHC.Exts
import Data.Data (Data(..), Typeable, mkConstr, mkDataType, constrIndex)
import qualified Data.Data as Data
import Control.Applicative( Applicative(..) )
import Data.IORef
import System.IO.Unsafe	( unsafePerformIO )
import Control.Monad (liftM)
import System.IO	( hPutStrLn, stderr )
import Data.Char        ( isAlpha, isAlphaNum, isUpper )
import Data.Word        ( Word8 )

-----------------------------------------------------
--
--		The Quasi class
--
-----------------------------------------------------

class (Monad m, Applicative m) => Quasi m where
  qNewName :: String -> m Name
	-- ^ Fresh names

	-- Error reporting and recovery
  qReport  :: Bool -> String -> m ()	-- ^ Report an error (True) or warning (False)
					-- ...but carry on; use 'fail' to stop
  qRecover :: m a -- ^ the error handler
           -> m a -- ^ action which may fail
           -> m a		-- ^ Recover from the monadic 'fail'

	-- Inspect the type-checker's environment
  qLookupName :: Bool -> String -> m (Maybe Name)
       -- True <=> type namespace, False <=> value namespace
  qReify          :: Name -> m Info
  qReifyInstances :: Name -> [Type] -> m [Dec]
       -- Is (n tys) an instance?
       -- Returns list of matching instance Decs
       --    (with empty sub-Decs)
       -- Works for classes and type functions
  qReifyRoles       :: Name -> m [Role]
  qReifyAnnotations :: Data a => AnnLookup -> m [a]
  qReifyModule      :: Module -> m ModuleInfo

  qLocation :: m Loc

  qRunIO :: IO a -> m a
  -- ^ Input/output (dangerous)

  qAddDependentFile :: FilePath -> m ()

  qAddTopDecls :: [Dec] -> m ()

  qAddModFinalizer :: Q () -> m ()

  qGetQ :: Typeable a => m (Maybe a)

  qPutQ :: Typeable a => a -> m ()

-----------------------------------------------------
--	The IO instance of Quasi
--
--  This instance is used only when running a Q
--  computation in the IO monad, usually just to
--  print the result.  There is no interesting
--  type environment, so reification isn't going to
--  work.
--
-----------------------------------------------------

instance Quasi IO where
  qNewName s = do { n <- readIORef counter
                 ; writeIORef counter (n+1)
                 ; return (mkNameU s n) }

  qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
  qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)

  qLookupName _ _     = badIO "lookupName"
  qReify _            = badIO "reify"
  qReifyInstances _ _ = badIO "reifyInstances"
  qReifyRoles _       = badIO "reifyRoles"
  qReifyAnnotations _ = badIO "reifyAnnotations"
  qReifyModule _      = badIO "reifyModule"
  qLocation    	      = badIO "currentLocation"
  qRecover _ _ 	      = badIO "recover" -- Maybe we could fix this?
  qAddDependentFile _ = badIO "addDependentFile"
  qAddTopDecls _      = badIO "addTopDecls"
  qAddModFinalizer _  = badIO "addModFinalizer"
  qGetQ               = badIO "getQ"
  qPutQ _             = badIO "putQ"

  qRunIO m = m

badIO :: String -> IO a
badIO op = do	{ qReport True ("Can't do `" ++ op ++ "' in the IO monad")
		; fail "Template Haskell failure" }

-- Global variable to generate unique symbols
counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)


-----------------------------------------------------
--
--		The Q monad
--
-----------------------------------------------------

newtype Q a = Q { unQ :: forall m. Quasi m => m a }

-- \"Runs\" the 'Q' monad. Normal users of Template Haskell
-- should not need this function, as the splice brackets @$( ... )@
-- are the usual way of running a 'Q' computation.
--
-- This function is primarily used in GHC internals, and for debugging
-- splices by running them in 'IO'.
--
-- Note that many functions in 'Q', such as 'reify' and other compiler
-- queries, are not supported when running 'Q' in 'IO'; these operations
-- simply fail at runtime. Indeed, the only operations guaranteed to succeed
-- are 'newName', 'runIO', 'reportError' and 'reportWarning'.
runQ :: Quasi m => Q a -> m a
runQ (Q m) = m

instance Monad Q where
  return x   = Q (return x)
  Q m >>= k  = Q (m >>= \x -> unQ (k x))
  Q m >> Q n = Q (m >> n)
  fail s     = report True s >> Q (fail "Q monad failure")

instance Functor Q where
  fmap f (Q x) = Q (fmap f x)

instance Applicative Q where
  pure x = Q (pure x)
  Q f <*> Q x = Q (f <*> x)

-----------------------------------------------------
--
--		The TExp type
--
-----------------------------------------------------

type role TExp nominal   -- See Note [Role of TExp]
newtype TExp a = TExp { unType :: Exp }

unTypeQ :: Q (TExp a) -> Q Exp
unTypeQ m = do { TExp e <- m
               ; return e }

unsafeTExpCoerce :: Q Exp -> Q (TExp a)
unsafeTExpCoerce m = do { e <- m
                        ; return (TExp e) }

{- Note [Role of TExp]
~~~~~~~~~~~~~~~~~~~~~~
TExp's argument must have a nominal role, not phantom as would
be inferred (Trac #8459).  Consider

  e :: TExp Age
  e = MkAge 3

  foo = $(coerce e) + 4::Int

The splice will evaluate to (MkAge 3) and you can't add that to
4::Int. So you can't coerce a (TExp Age) to a (TExp Int). -}

----------------------------------------------------
-- Packaged versions for the programmer, hiding the Quasi-ness

{- |
Generate a fresh name, which cannot be captured.

For example, this:

@f = $(do
  nm1 <- newName \"x\"
  let nm2 = 'mkName' \"x\"
  return ('LamE' ['VarP' nm1] (LamE [VarP nm2] ('VarE' nm1)))
 )@

will produce the splice

>f = \x0 -> \x -> x0

In particular, the occurrence @VarE nm1@ refers to the binding @VarP nm1@,
and is not captured by the binding @VarP nm2@.

Although names generated by @newName@ cannot /be captured/, they can
/capture/ other names. For example, this:

>g = $(do
>  nm1 <- newName "x"
>  let nm2 = mkName "x"
>  return (LamE [VarP nm2] (LamE [VarP nm1] (VarE nm2)))
> )

will produce the splice

>g = \x -> \x0 -> x0

since the occurrence @VarE nm2@ is captured by the innermost binding
of @x@, namely @VarP nm1@.
-}
newName :: String -> Q Name
newName s = Q (qNewName s)

-- | Report an error (True) or warning (False),
-- but carry on; use 'fail' to stop.
report  :: Bool -> String -> Q ()
report b s = Q (qReport b s)
{-# DEPRECATED report "Use reportError or reportWarning instead" #-} -- deprecated in 7.6

-- | Report an error to the user, but allow the current splice's computation to carry on. To abort the computation, use 'fail'.
reportError :: String -> Q ()
reportError = report True

-- | Report a warning to the user, and carry on.
reportWarning :: String -> Q ()
reportWarning = report False

-- | Recover from errors raised by 'reportError' or 'fail'.
recover :: Q a -- ^ handler to invoke on failure
        -> Q a -- ^ computation to run
        -> Q a
recover (Q r) (Q m) = Q (qRecover r m)

-- We don't export lookupName; the Bool isn't a great API
-- Instead we export lookupTypeName, lookupValueName
lookupName :: Bool -> String -> Q (Maybe Name)
lookupName ns s = Q (qLookupName ns s)

-- | Look up the given name in the (type namespace of the) current splice's scope. See "Language.Haskell.TH.Syntax#namelookup" for more details.
lookupTypeName :: String -> Q (Maybe Name)
lookupTypeName  s = Q (qLookupName True s)

-- | Look up the given name in the (value namespace of the) current splice's scope. See "Language.Haskell.TH.Syntax#namelookup" for more details.
lookupValueName :: String -> Q (Maybe Name)
lookupValueName s = Q (qLookupName False s)

{-
Note [Name lookup]
~~~~~~~~~~~~~~~~~~
-}
{- $namelookup #namelookup#
The functions 'lookupTypeName' and 'lookupValueName' provide
a way to query the current splice's context for what names
are in scope. The function 'lookupTypeName' queries the type
namespace, whereas 'lookupValueName' queries the value namespace,
but the functions are otherwise identical.

A call @lookupValueName s@ will check if there is a value
with name @s@ in scope at the current splice's location. If
there is, the @Name@ of this value is returned;
if not, then @Nothing@ is returned.

The returned name cannot be \"captured\".
For example:

> f = "global"
> g = $( do
>          Just nm <- lookupValueName "f"
>          [| let f = "local" in $( varE nm ) |]

In this case, @g = \"global\"@; the call to @lookupValueName@
returned the global @f@, and this name was /not/ captured by
the local definition of @f@.

The lookup is performed in the context of the /top-level/ splice
being run. For example:

> f = "global"
> g = $( [| let f = "local" in
>            $(do
>                Just nm <- lookupValueName "f"
>                varE nm
>             ) |] )

Again in this example, @g = \"global\"@, because the call to
@lookupValueName@ queries the context of the outer-most @$(...)@.

Operators should be queried without any surrounding parentheses, like so:

> lookupValueName "+"

Qualified names are also supported, like so:

> lookupValueName "Prelude.+"
> lookupValueName "Prelude.map"

-}


{- | 'reify' looks up information about the 'Name'.

It is sometimes useful to construct the argument name using 'lookupTypeName' or 'lookupValueName'
to ensure that we are reifying from the right namespace. For instance, in this context:

> data D = D

which @D@ does @reify (mkName \"D\")@ return information about? (Answer: @D@-the-type, but don't rely on it.)
To ensure we get information about @D@-the-value, use 'lookupValueName':

> do
>   Just nm <- lookupValueName "D"
>   reify nm

and to get information about @D@-the-type, use 'lookupTypeName'.
-}
reify :: Name -> Q Info
reify v = Q (qReify v)

{- | @reifyInstances nm tys@ returns a list of visible instances of @nm tys@. That is,
if @nm@ is the name of a type class, then all instances of this class at the types @tys@
are returned. Alternatively, if @nm@ is the name of a data family or type family,
all instances of this family at the types @tys@ are returned.
-}
reifyInstances :: Name -> [Type] -> Q [InstanceDec]
reifyInstances cls tys = Q (qReifyInstances cls tys)

{- | @reifyRoles nm@ returns the list of roles associated with the parameters of
the tycon @nm@. Fails if @nm@ cannot be found or is not a tycon.
The returned list should never contain 'InferR'.
-}
reifyRoles :: Name -> Q [Role]
reifyRoles nm = Q (qReifyRoles nm)

-- | @reifyAnnotations target@ returns the list of annotations
-- associated with @target@.  Only the annotations that are
-- appropriately typed is returned.  So if you have @Int@ and @String@
-- annotations for the same target, you have to call this function twice.
reifyAnnotations :: Data a => AnnLookup -> Q [a]
reifyAnnotations an = Q (qReifyAnnotations an)

-- | @reifyModule mod@ looks up information about module @mod@.  To
-- look up the current module, call this function with the return
-- value of @thisModule@.
reifyModule :: Module -> Q ModuleInfo
reifyModule m = Q (qReifyModule m)

-- | Is the list of instances returned by 'reifyInstances' nonempty?
isInstance :: Name -> [Type] -> Q Bool
isInstance nm tys = do { decs <- reifyInstances nm tys
                       ; return (not (null decs)) }

-- | The location at which this computation is spliced.
location :: Q Loc
location = Q qLocation

-- |The 'runIO' function lets you run an I\/O computation in the 'Q' monad.
-- Take care: you are guaranteed the ordering of calls to 'runIO' within
-- a single 'Q' computation, but not about the order in which splices are run.
--
-- Note: for various murky reasons, stdout and stderr handles are not
-- necesarily flushed when the  compiler finishes running, so you should
-- flush them yourself.
runIO :: IO a -> Q a
runIO m = Q (qRunIO m)

-- | Record external files that runIO is using (dependent upon).
-- The compiler can then recognize that it should re-compile the file using this TH when the external file changes.
-- Note that ghc -M will still not know about these dependencies - it does not execute TH.
-- Expects an absolute file path.
addDependentFile :: FilePath -> Q ()
addDependentFile fp = Q (qAddDependentFile fp)

-- | Add additional top-level declarations. The added declarations will be type
-- checked along with the current declaration group.
addTopDecls :: [Dec] -> Q ()
addTopDecls ds = Q (qAddTopDecls ds)

-- | Add a finalizer that will run in the Q monad after the current module has
-- been type checked. This only makes sense when run within a top-level splice.
addModFinalizer :: Q () -> Q ()
addModFinalizer act = Q (qAddModFinalizer (unQ act))

-- | Get state from the Q monad.
getQ :: Typeable a => Q (Maybe a)
getQ = Q qGetQ

-- | Replace the state in the Q monad.
putQ :: Typeable a => a -> Q ()
putQ x = Q (qPutQ x)

instance Quasi Q where
  qNewName  	    = newName
  qReport   	    = report
  qRecover  	    = recover
  qReify    	    = reify
  qReifyInstances   = reifyInstances
  qReifyRoles       = reifyRoles
  qReifyAnnotations = reifyAnnotations
  qReifyModule      = reifyModule
  qLookupName       = lookupName
  qLocation 	    = location
  qRunIO    	    = runIO
  qAddDependentFile = addDependentFile
  qAddTopDecls      = addTopDecls
  qAddModFinalizer  = addModFinalizer
  qGetQ             = getQ
  qPutQ             = putQ


----------------------------------------------------
-- The following operations are used solely in DsMeta when desugaring brackets
-- They are not necessary for the user, who can use ordinary return and (>>=) etc

returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

sequenceQ :: [Q a] -> Q [a]
sequenceQ = sequence


-----------------------------------------------------
--
--		The Lift class
--
-----------------------------------------------------

class Lift t where
  lift :: t -> Q Exp

instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x= return (LitE (IntegerL (fromIntegral x)))

instance Lift Char where
  lift x = return (LitE (CharL x))

instance Lift Bool where
  lift True  = return (ConE trueName)
  lift False = return (ConE falseName)

instance Lift a => Lift (Maybe a) where
  lift Nothing  = return (ConE nothingName)
  lift (Just x) = liftM (ConE justName `AppE`) (lift x)

instance (Lift a, Lift b) => Lift (Either a b) where
  lift (Left x)  = liftM (ConE leftName  `AppE`) (lift x)
  lift (Right y) = liftM (ConE rightName `AppE`) (lift y)

instance Lift a => Lift [a] where
  lift xs = do { xs' <- mapM lift xs; return (ListE xs') }

liftString :: String -> Q Exp
-- Used in TcExpr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b)
    = liftM TupE $ sequence [lift a, lift b]

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c)
    = liftM TupE $ sequence [lift a, lift b, lift c]

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d]

instance (Lift a, Lift b, Lift c, Lift d, Lift e)
      => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f)
      => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f]

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g)
      => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g)
    = liftM TupE $ sequence [lift a, lift b, lift c, lift d, lift e, lift f, lift g]

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker
--     creates it.
{-# RULES "TH:liftString" lift = \s -> return (LitE (StringL s)) #-}


trueName, falseName :: Name
trueName  = mkNameG DataName "ghc-prim" "GHC.Types" "True"
falseName = mkNameG DataName "ghc-prim" "GHC.Types" "False"

nothingName, justName :: Name
nothingName = mkNameG DataName "base" "Data.Maybe" "Nothing"
justName    = mkNameG DataName "base" "Data.Maybe" "Just"

leftName, rightName :: Name
leftName  = mkNameG DataName "base" "Data.Either" "Left"
rightName = mkNameG DataName "base" "Data.Either" "Right"


-----------------------------------------------------
--		Names and uniques
-----------------------------------------------------

newtype ModName = ModName String	-- Module name
 deriving (Show,Eq,Ord,Typeable,Data)

newtype PkgName = PkgName String	-- package name
 deriving (Show,Eq,Ord,Typeable,Data)

-- | Obtained from 'reifyModule' and 'thisModule'.
data Module = Module PkgName ModName -- package qualified module name
 deriving (Show,Eq,Ord,Typeable,Data)

newtype OccName = OccName String
 deriving (Show,Eq,Ord,Typeable,Data)

mkModName :: String -> ModName
mkModName s = ModName s

modString :: ModName -> String
modString (ModName m) = m


mkPkgName :: String -> PkgName
mkPkgName s = PkgName s

pkgString :: PkgName -> String
pkgString (PkgName m) = m


-----------------------------------------------------
--		OccName
-----------------------------------------------------

mkOccName :: String -> OccName
mkOccName s = OccName s

occString :: OccName -> String
occString (OccName occ) = occ


-----------------------------------------------------
--		 Names
-----------------------------------------------------
--
-- For "global" names ('NameG') we need a totally unique name,
-- so we must include the name-space of the thing
--
-- For unique-numbered things ('NameU'), we've got a unique reference
-- anyway, so no need for name space
--
-- For dynamically bound thing ('NameS') we probably want them to
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
--
-- > let v = mkName "T" in [| data $v = $v |]
--
-- Here we use the same Name for both type constructor and data constructor
--
--
-- NameL and NameG are bound *outside* the TH syntax tree
-- either globally (NameG) or locally (NameL). Ex:
--
-- > f x = $(h [| (map, x) |])
--
-- The 'map' will be a NameG, and 'x' wil be a NameL
--
-- These Names should never appear in a binding position in a TH syntax tree

{- $namecapture #namecapture#
Much of 'Name' API is concerned with the problem of /name capture/, which
can be seen in the following example.

> f expr = [| let x = 0 in $expr |]
> ...
> g x = $( f [| x |] )
> h y = $( f [| y |] )

A naive desugaring of this would yield:

> g x = let x = 0 in x
> h y = let x = 0 in y

All of a sudden, @g@ and @h@ have different meanings! In this case,
we say that the @x@ in the RHS of @g@ has been /captured/
by the binding of @x@ in @f@.

What we actually want is for the @x@ in @f@ to be distinct from the
@x@ in @g@, so we get the following desugaring:

> g x = let x' = 0 in x
> h y = let x' = 0 in y

which avoids name capture as desired.

In the general case, we say that a @Name@ can be captured if
the thing it refers to can be changed by adding new declarations.
-}

{- |
An abstract type representing names in the syntax tree.

'Name's can be constructed in several ways, which come with different
name-capture guarantees (see "Language.Haskell.TH.Syntax#namecapture" for
an explanation of name capture):

  * the built-in syntax @'f@ and @''T@ can be used to construct names,
    The expression @'f@ gives a @Name@ which refers to the value @f@
    currently in scope, and @''T@ gives a @Name@ which refers to the
    type @T@ currently in scope. These names can never be captured.

  * 'lookupValueName' and 'lookupTypeName' are similar to @'f@ and
     @''T@ respectively, but the @Name@s are looked up at the point
     where the current splice is being run. These names can never be
     captured.

  * 'newName' monadically generates a new name, which can never
     be captured.

  * 'mkName' generates a capturable name.

Names constructed using @newName@ and @mkName@ may be used in bindings
(such as @let x = ...@ or @\x -> ...@), but names constructed using
@lookupValueName@, @lookupTypeName@, @'f@, @''T@ may not.
-}
data Name = Name OccName NameFlavour deriving (Typeable, Data)

data NameFlavour
  = NameS           -- ^ An unqualified name; dynamically bound
  | NameQ ModName   -- ^ A qualified name; dynamically bound
  | NameU Int#      -- ^ A unique local name
  | NameL Int#      -- ^ Local name bound outside of the TH AST
  | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
                -- An original name (occurrences only, not binders)
		-- Need the namespace too to be sure which
		-- thing we are naming
  deriving ( Typeable )

-- |
-- Although the NameFlavour type is abstract, the Data instance is not. The reason for this
-- is that currently we use Data to serialize values in annotations, and in order for that to
-- work for Template Haskell names introduced via the 'x syntax we need gunfold on NameFlavour
-- to work. Bleh!
--
-- The long term solution to this is to use the binary package for annotation serialization and
-- then remove this instance. However, to do _that_ we need to wait on binary to become stable, since
-- boot libraries cannot be upgraded separately from GHC itself.
--
-- This instance cannot be derived automatically due to bug #2701
instance Data NameFlavour where
     gfoldl _ z NameS          = z NameS
     gfoldl k z (NameQ mn)     = z NameQ `k` mn
     gfoldl k z (NameU i)      = z (\(I# i') -> NameU i') `k` (I# i)
     gfoldl k z (NameL i)      = z (\(I# i') -> NameL i') `k` (I# i)
     gfoldl k z (NameG ns p m) = z NameG `k` ns `k` p `k` m
     gunfold k z c = case constrIndex c of
         1 -> z NameS
         2 -> k $ z NameQ
         3 -> k $ z (\(I# i) -> NameU i)
         4 -> k $ z (\(I# i) -> NameL i)
         5 -> k $ k $ k $ z NameG
         _ -> error "gunfold: NameFlavour"
     toConstr NameS = con_NameS
     toConstr (NameQ _) = con_NameQ
     toConstr (NameU _) = con_NameU
     toConstr (NameL _) = con_NameL
     toConstr (NameG _ _ _) = con_NameG
     dataTypeOf _ = ty_NameFlavour

con_NameS, con_NameQ, con_NameU, con_NameL, con_NameG :: Data.Constr
con_NameS = mkConstr ty_NameFlavour "NameS" [] Data.Prefix
con_NameQ = mkConstr ty_NameFlavour "NameQ" [] Data.Prefix
con_NameU = mkConstr ty_NameFlavour "NameU" [] Data.Prefix
con_NameL = mkConstr ty_NameFlavour "NameL" [] Data.Prefix
con_NameG = mkConstr ty_NameFlavour "NameG" [] Data.Prefix

ty_NameFlavour :: Data.DataType
ty_NameFlavour = mkDataType "Language.Haskell.TH.Syntax.NameFlavour"
                            [con_NameS, con_NameQ, con_NameU,
                             con_NameL, con_NameG]

data NameSpace = VarName	-- ^ Variables
	       | DataName	-- ^ Data constructors
	       | TcClsName	-- ^ Type constructors and classes; Haskell has them
				-- in the same name space for now.
	       deriving( Eq, Ord, Data, Typeable )

type Uniq = Int

-- | The name without its module prefix
nameBase :: Name -> String
nameBase (Name occ _) = occString occ

-- | Module prefix of a name, if it exists
nameModule :: Name -> Maybe String
nameModule (Name _ (NameQ m))     = Just (modString m)
nameModule (Name _ (NameG _ _ m)) = Just (modString m)
nameModule _                      = Nothing

{- |
Generate a capturable name. Occurrences of such names will be
resolved according to the Haskell scoping rules at the occurrence
site.

For example:

> f = [| pi + $(varE (mkName "pi")) |]
> ...
> g = let pi = 3 in $f

In this case, @g@ is desugared to

> g = Prelude.pi + 3

Note that @mkName@ may be used with qualified names:

> mkName "Prelude.pi"

See also 'Language.Haskell.TH.Lib.dyn' for a useful combinator. The above example could
be rewritten using 'dyn' as

> f = [| pi + $(dyn "pi") |]
-}
mkName :: String -> Name
-- The string can have a '.', thus "Foo.baz",
-- giving a dynamically-bound qualified name,
-- in which case we want to generate a NameQ
--
-- Parse the string to see if it has a "." in it
-- so we know whether to generate a qualified or unqualified name
-- It's a bit tricky because we need to parse
--
-- > Foo.Baz.x   as    Qual Foo.Baz x
--
-- So we parse it from back to front
mkName str
  = split [] (reverse str)
  where
    split occ []        = Name (mkOccName occ) NameS
    split occ ('.':rev)	| not (null occ)
			, is_rev_mod_name rev
			= Name (mkOccName occ) (NameQ (mkModName (reverse rev)))
	-- The 'not (null occ)' guard ensures that
	-- 	mkName "&." = Name "&." NameS
	-- The 'is_rev_mod' guards ensure that
	--	mkName ".&" = Name ".&" NameS
	--	mkName "^.." = Name "^.." NameS      -- Trac #8633
	--	mkName "Data.Bits..&" = Name ".&" (NameQ "Data.Bits")
	-- This rather bizarre case actually happened; (.&.) is in Data.Bits
    split occ (c:rev)   = split (c:occ) rev

    -- Recognises a reversed module name xA.yB.C, 
    -- with at least one component, 
    -- and each component looks like a module name
    --   (i.e. non-empty, starts with capital, all alpha)
    is_rev_mod_name rev_mod_str
      | (compt, rest) <- break (== '.') rev_mod_str
      , not (null compt), isUpper (last compt), all is_mod_char compt
      = case rest of
          []             -> True
          (_dot : rest') -> is_rev_mod_name rest'
      | otherwise
      = False

    is_mod_char c = isAlphaNum c || c == '_' || c == '\''

-- | Only used internally
mkNameU :: String -> Uniq -> Name
mkNameU s (I# u) = Name (mkOccName s) (NameU u)

-- | Only used internally
mkNameL :: String -> Uniq -> Name
mkNameL s (I# u) = Name (mkOccName s) (NameL u)

-- | Used for 'x etc, but not available to the programmer
mkNameG :: NameSpace -> String -> String -> String -> Name
mkNameG ns pkg modu occ
  = Name (mkOccName occ) (NameG ns (mkPkgName pkg) (mkModName modu))

mkNameG_v, mkNameG_tc, mkNameG_d :: String -> String -> String -> Name
mkNameG_v  = mkNameG VarName
mkNameG_tc = mkNameG TcClsName
mkNameG_d  = mkNameG DataName

instance Eq Name where
  v1 == v2 = cmpEq (v1 `compare` v2)

instance Ord Name where
  (Name o1 f1) `compare` (Name o2 f2) = (f1 `compare` f2)   `thenCmp`
				        (o1 `compare` o2)

instance Eq NameFlavour where
  f1 == f2 = cmpEq (f1 `compare` f2)

instance Ord NameFlavour where
	-- NameS < NameQ < NameU < NameL < NameG
  NameS `compare` NameS = EQ
  NameS `compare` _     = LT

  (NameQ _)  `compare` NameS      = GT
  (NameQ m1) `compare` (NameQ m2) = m1 `compare` m2
  (NameQ _)  `compare` _          = LT

  (NameU _)  `compare` NameS      = GT
  (NameU _)  `compare` (NameQ _)  = GT
  (NameU u1) `compare` (NameU u2) | isTrue# (u1  <# u2) = LT
				  | isTrue# (u1 ==# u2) = EQ
				  | otherwise           = GT
  (NameU _)  `compare` _     = LT

  (NameL _)  `compare` NameS      = GT
  (NameL _)  `compare` (NameQ _)  = GT
  (NameL _)  `compare` (NameU _)  = GT
  (NameL u1) `compare` (NameL u2) | isTrue# (u1  <# u2) = LT
				  | isTrue# (u1 ==# u2) = EQ
				  | otherwise           = GT
  (NameL _)  `compare` _          = LT

  (NameG ns1 p1 m1) `compare` (NameG ns2 p2 m2) = (ns1 `compare` ns2) `thenCmp`
                                            (p1 `compare` p2) `thenCmp`
					    (m1 `compare` m2)
  (NameG _ _ _)    `compare` _ = GT

data NameIs = Alone | Applied | Infix

showName :: Name -> String
showName = showName' Alone

showName' :: NameIs -> Name -> String
showName' ni nm
 = case ni of
       Alone        -> nms
       Applied
        | pnam      -> nms
        | otherwise -> "(" ++ nms ++ ")"
       Infix
        | pnam      -> "`" ++ nms ++ "`"
        | otherwise -> nms
    where
	-- For now, we make the NameQ and NameG print the same, even though
	-- NameQ is a qualified name (so what it means depends on what the
	-- current scope is), and NameG is an original name (so its meaning
	-- should be independent of what's in scope.
	-- We may well want to distinguish them in the end.
	-- Ditto NameU and NameL
        nms = case nm of
                    Name occ NameS         -> occString occ
                    Name occ (NameQ m)     -> modString m ++ "." ++ occString occ
                    Name occ (NameG _ _ m) -> modString m ++ "." ++ occString occ
                    Name occ (NameU u)     -> occString occ ++ "_" ++ show (I# u)
                    Name occ (NameL u)     -> occString occ ++ "_" ++ show (I# u)

        pnam = classify nms

        -- True if we are function style, e.g. f, [], (,)
        -- False if we are operator style, e.g. +, :+
        classify "" = False -- shouldn't happen; . operator is handled below
        classify (x:xs) | isAlpha x || (x `elem` "_[]()") =
                            case dropWhile (/='.') xs of
                                  (_:xs') -> classify xs'
                                  []      -> True
                        | otherwise = False

instance Show Name where
  show = showName

-- Tuple data and type constructors
-- | Tuple data constructor
tupleDataName :: Int -> Name
-- | Tuple type constructor
tupleTypeName :: Int -> Name

tupleDataName 0 = mk_tup_name 0 DataName
tupleDataName 1 = error "tupleDataName 1"
tupleDataName n = mk_tup_name (n-1) DataName

tupleTypeName 0 = mk_tup_name 0 TcClsName
tupleTypeName 1 = error "tupleTypeName 1"
tupleTypeName n = mk_tup_name (n-1) TcClsName

mk_tup_name :: Int -> NameSpace -> Name
mk_tup_name n_commas space
  = Name occ (NameG space (mkPkgName "ghc-prim") tup_mod)
  where
    occ = mkOccName ('(' : replicate n_commas ',' ++ ")")
    tup_mod = mkModName "GHC.Tuple"

-- Unboxed tuple data and type constructors
-- | Unboxed tuple data constructor
unboxedTupleDataName :: Int -> Name
-- | Unboxed tuple type constructor
unboxedTupleTypeName :: Int -> Name

unboxedTupleDataName 0 = error "unboxedTupleDataName 0"
unboxedTupleDataName 1 = error "unboxedTupleDataName 1"
unboxedTupleDataName n = mk_unboxed_tup_name (n-1) DataName

unboxedTupleTypeName 0 = error "unboxedTupleTypeName 0"
unboxedTupleTypeName 1 = error "unboxedTupleTypeName 1"
unboxedTupleTypeName n = mk_unboxed_tup_name (n-1) TcClsName

mk_unboxed_tup_name :: Int -> NameSpace -> Name
mk_unboxed_tup_name n_commas space
  = Name occ (NameG space (mkPkgName "ghc-prim") tup_mod)
  where
    occ = mkOccName ("(#" ++ replicate n_commas ',' ++ "#)")
    tup_mod = mkModName "GHC.Tuple"



-----------------------------------------------------
--		Locations
-----------------------------------------------------

data Loc
  = Loc { loc_filename :: String
	, loc_package  :: String
	, loc_module   :: String
	, loc_start    :: CharPos
	, loc_end      :: CharPos }

type CharPos = (Int, Int)	-- ^ Line and character position


-----------------------------------------------------
--
--	The Info returned by reification
--
-----------------------------------------------------

-- | Obtained from 'reify' in the 'Q' Monad.
data Info
  =
  -- | A class, with a list of its visible instances
  ClassI
      Dec
      [InstanceDec]

  -- | A class method
  | ClassOpI
       Name
       Type
       ParentName
       Fixity

  -- | A \"plain\" type constructor. \"Fancier\" type constructors are returned using 'PrimTyConI' or 'FamilyI' as appropriate
  | TyConI
        Dec

  -- | A type or data family, with a list of its visible instances. A closed
  -- type family is returned with 0 instances.
  | FamilyI
        Dec
        [InstanceDec]

  -- | A \"primitive\" type constructor, which can't be expressed with a 'Dec'. Examples: @(->)@, @Int#@.
  | PrimTyConI
       Name
       Arity
       Unlifted

  -- | A data constructor
  | DataConI
       Name
       Type
       ParentName
       Fixity

  {- |
  A \"value\" variable (as opposed to a type variable, see 'TyVarI').

  The @Maybe Dec@ field contains @Just@ the declaration which
  defined the variable -- including the RHS of the declaration --
  or else @Nothing@, in the case where the RHS is unavailable to
  the compiler. At present, this value is _always_ @Nothing@:
  returning the RHS has not yet been implemented because of
  lack of interest.
  -}
  | VarI
       Name
       Type
       (Maybe Dec)
       Fixity

  {- |
  A type variable.

  The @Type@ field contains the type which underlies the variable.
  At present, this is always @'VarT' theName@, but future changes
  may permit refinement of this.
  -}
  | TyVarI 	-- Scoped type variable
	Name
	Type	-- What it is bound to
  deriving( Show, Data, Typeable )

-- | Obtained from 'reifyModule' in the 'Q' Monad.
data ModuleInfo =
  -- | Contains the import list of the module.
  ModuleInfo [Module]
  deriving( Show, Data, Typeable )

{- |
In 'ClassOpI' and 'DataConI', name of the parent class or type
-}
type ParentName = Name

-- | In 'PrimTyConI', arity of the type constructor
type Arity = Int

-- | In 'PrimTyConI', is the type constructor unlifted?
type Unlifted = Bool

-- | 'InstanceDec' desribes a single instance of a class or type function.
-- It is just a 'Dec', but guaranteed to be one of the following:
--
--   * 'InstanceD' (with empty @['Dec']@)
--
--   * 'DataInstD' or 'NewtypeInstD' (with empty derived @['Name']@)
--
--   * 'TySynInstD'
type InstanceDec = Dec

data Fixity          = Fixity Int FixityDirection
    deriving( Eq, Show, Data, Typeable )
data FixityDirection = InfixL | InfixR | InfixN
    deriving( Eq, Show, Data, Typeable )

-- | Highest allowed operator precedence for 'Fixity' constructor (answer: 9)
maxPrecedence :: Int
maxPrecedence = (9::Int)

-- | Default fixity: @infixl 9@
defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL


{-
Note [Unresolved infix]
~~~~~~~~~~~~~~~~~~~~~~~
-}
{- $infix #infix#
When implementing antiquotation for quasiquoters, one often wants
to parse strings into expressions:

> parse :: String -> Maybe Exp

But how should we parse @a + b * c@? If we don't know the fixities of
@+@ and @*@, we don't know whether to parse it as @a + (b * c)@ or @(a
+ b) * c@.

In cases like this, use 'UInfixE' or 'UInfixP', which stand for
\"unresolved infix expression\" and \"unresolved infix pattern\". When
the compiler is given a splice containing a tree of @UInfixE@
applications such as

> UInfixE
>   (UInfixE e1 op1 e2)
>   op2
>   (UInfixE e3 op3 e4)

it will look up and the fixities of the relevant operators and
reassociate the tree as necessary.

  * trees will not be reassociated across 'ParensE' or 'ParensP',
    which are of use for parsing expressions like

    > (a + b * c) + d * e

  * 'InfixE' and 'InfixP' expressions are never reassociated.

  * The 'UInfixE' constructor doesn't support sections. Sections
    such as @(a *)@ have no ambiguity, so 'InfixE' suffices. For longer
    sections such as @(a + b * c -)@, use an 'InfixE' constructor for the
    outer-most section, and use 'UInfixE' constructors for all
    other operators:

    > InfixE
    >   Just (UInfixE ...a + b * c...)
    >   op
    >   Nothing

    Sections such as @(a + b +)@ and @((a + b) +)@ should be rendered
    into 'Exp's differently:

    > (+ a + b)   ---> InfixE Nothing + (Just $ UInfixE a + b)
    >                    -- will result in a fixity error if (+) is left-infix
    > (+ (a + b)) ---> InfixE Nothing + (Just $ ParensE $ UInfixE a + b)
    >                    -- no fixity errors

  * Quoted expressions such as

    > [| a * b + c |] :: Q Exp
    > [p| a : b : c |] :: Q Pat

    will never contain 'UInfixE', 'UInfixP', 'ParensE', or 'ParensP'
    constructors.

-}

-----------------------------------------------------
--
--	The main syntax data types
--
-----------------------------------------------------

data Lit = CharL Char
         | StringL String
         | IntegerL Integer     -- ^ Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | WordPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
         | StringPrimL [Word8]	-- ^ A primitive C-style string, type Addr#
    deriving( Show, Eq, Data, Typeable )

    -- We could add Int, Float, Double etc, as we do in HsLit,
    -- but that could complicate the
    -- suppposedly-simple TH.Syntax literal type

-- | Pattern in Haskell given in @{}@
data Pat
  = LitP Lit                      -- ^ @{ 5 or 'c' }@
  | VarP Name                     -- ^ @{ x }@
  | TupP [Pat]                    -- ^ @{ (p1,p2) }@
  | UnboxedTupP [Pat]             -- ^ @{ (# p1,p2 #) }@
  | ConP Name [Pat]               -- ^ @data T1 = C1 t1 t2; {C1 p1 p1} = e@
  | InfixP Pat Name Pat           -- ^ @foo ({x :+ y}) = e@
  | UInfixP Pat Name Pat          -- ^ @foo ({x :+ y}) = e@
                                  --
                                  -- See "Language.Haskell.TH.Syntax#infix"
  | ParensP Pat                   -- ^ @{(p)}@
                                  --
                                  -- See "Language.Haskell.TH.Syntax#infix"
  | TildeP Pat                    -- ^ @{ ~p }@
  | BangP Pat                     -- ^ @{ !p }@
  | AsP Name Pat                  -- ^ @{ x \@ p }@
  | WildP                         -- ^ @{ _ }@
  | RecP Name [FieldPat]          -- ^ @f (Pt { pointx = x }) = g x@
  | ListP [ Pat ]                 -- ^ @{ [1,2,3] }@
  | SigP Pat Type                 -- ^ @{ p :: t }@
  | ViewP Exp Pat                 -- ^ @{ e -> p }@
  deriving( Show, Eq, Data, Typeable )

type FieldPat = (Name,Pat)

data Match = Match Pat Body [Dec] -- ^ @case e of { pat -> body where decs }@
    deriving( Show, Eq, Data, Typeable )
data Clause = Clause [Pat] Body [Dec]
                                  -- ^ @f { p1 p2 = body where decs }@
    deriving( Show, Eq, Data, Typeable )

data Exp
  = VarE Name                          -- ^ @{ x }@
  | ConE Name                          -- ^ @data T1 = C1 t1 t2; p = {C1} e1 e2  @
  | LitE Lit                           -- ^ @{ 5 or 'c'}@
  | AppE Exp Exp                       -- ^ @{ f x }@

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- ^ @{x + y} or {(x+)} or {(+ x)} or {(+)}@

    -- It's a bit gruesome to use an Exp as the
    -- operator, but how else can we distinguish
    -- constructors from non-constructors?
    -- Maybe there should be a var-or-con type?
    -- Or maybe we should leave it to the String itself?

  | UInfixE Exp Exp Exp                -- ^ @{x + y}@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | ParensE Exp                        -- ^ @{ (e) }@
                                       --
                                       -- See "Language.Haskell.TH.Syntax#infix"
  | LamE [Pat] Exp                     -- ^ @{ \ p1 p2 -> e }@
  | LamCaseE [Match]                   -- ^ @{ \case m1; m2 }@
  | TupE [Exp]                         -- ^ @{ (e1,e2) }  @
  | UnboxedTupE [Exp]                  -- ^ @{ (# e1,e2 #) }  @
  | CondE Exp Exp Exp                  -- ^ @{ if e1 then e2 else e3 }@
  | MultiIfE [(Guard, Exp)]            -- ^ @{ if | g1 -> e1 | g2 -> e2 }@
  | LetE [Dec] Exp                     -- ^ @{ let x=e1;   y=e2 in e3 }@
  | CaseE Exp [Match]                  -- ^ @{ case e of m1; m2 }@
  | DoE [Stmt]                         -- ^ @{ do { p <- e1; e2 }  }@
  | CompE [Stmt]                       -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@
      --
      -- The result expression of the comprehension is
      -- the /last/ of the @'Stmt'@s, and should be a 'NoBindS'.
      --
      -- E.g. translation:
      --
      -- > [ f x | x <- xs ]
      --
      -- > CompE [BindS (VarP x) (VarE xs), NoBindS (AppE (VarE f) (VarE x))]

  | ArithSeqE Range                    -- ^ @{ [ 1 ,2 .. 10 ] }@
  | ListE [ Exp ]                      -- ^ @{ [1,2,3] }@
  | SigE Exp Type                      -- ^ @{ e :: t }@
  | RecConE Name [FieldExp]            -- ^ @{ T { x = y, z = w } }@
  | RecUpdE Exp [FieldExp]             -- ^ @{ (f x) { z = w } }@
  deriving( Show, Eq, Data, Typeable )

type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

data Body
  = GuardedB [(Guard,Exp)]   -- ^ @f p { | e1 = e2
                                 --      | e3 = e4 }
                                 -- where ds@
  | NormalB Exp              -- ^ @f p { = e } where ds@
  deriving( Show, Eq, Data, Typeable )

data Guard
  = NormalG Exp -- ^ @f x { | odd x } = x@
  | PatG [Stmt] -- ^ @f x { | Just y <- x, Just z <- y } = z@
  deriving( Show, Eq, Data, Typeable )

data Stmt
  = BindS Pat Exp
  | LetS [ Dec ]
  | NoBindS Exp
  | ParS [[Stmt]]
  deriving( Show, Eq, Data, Typeable )

data Range = FromR Exp | FromThenR Exp Exp
           | FromToR Exp Exp | FromThenToR Exp Exp Exp
          deriving( Show, Eq, Data, Typeable )

data Dec
  = FunD Name [Clause]            -- ^ @{ f p1 p2 = b where decs }@
  | ValD Pat Body [Dec]           -- ^ @{ p = b where decs }@
  | DataD Cxt Name [TyVarBndr]
         [Con] [Name]             -- ^ @{ data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)}@
  | NewtypeD Cxt Name [TyVarBndr]
         Con [Name]               -- ^ @{ newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W)}@
  | TySynD Name [TyVarBndr] Type  -- ^ @{ type T x = (x,x) }@
  | ClassD Cxt Name [TyVarBndr]
         [FunDep] [Dec]           -- ^ @{ class Eq a => Ord a where ds }@
  | InstanceD Cxt Type [Dec]      -- ^ @{ instance Show w => Show [w]
                                  --       where ds }@
  | SigD Name Type                -- ^ @{ length :: [a] -> Int }@
  | ForeignD Foreign              -- ^ @{ foreign import ... }
                                  --{ foreign export ... }@

  | InfixD Fixity Name            -- ^ @{ infix 3 foo }@

  -- | pragmas
  | PragmaD Pragma                -- ^ @{ {\-# INLINE [1] foo #-\} }@

  -- | type families (may also appear in [Dec] of 'ClassD' and 'InstanceD')
  | FamilyD FamFlavour Name
         [TyVarBndr] (Maybe Kind) -- ^ @{ type family T a b c :: * }@

  | DataInstD Cxt Name [Type]
         [Con] [Name]             -- ^ @{ data instance Cxt x => T [x] = A x
                                  --                                | B (T x)
                                  --       deriving (Z,W)}@
  | NewtypeInstD Cxt Name [Type]
         Con [Name]               -- ^ @{ newtype instance Cxt x => T [x] = A (B x)
                                  --       deriving (Z,W)}@
  | TySynInstD Name TySynEqn      -- ^ @{ type instance ... }@

  | ClosedTypeFamilyD Name
      [TyVarBndr] (Maybe Kind)
      [TySynEqn]                  -- ^ @{ type family F a b :: * where ... }@

  | RoleAnnotD Name [Role]        -- ^ @{ type role T nominal representational }@
  deriving( Show, Eq, Data, Typeable )

-- | One equation of a type family instance or closed type family. The
-- arguments are the left-hand-side type patterns and the right-hand-side
-- result.
data TySynEqn = TySynEqn [Type] Type
  deriving( Show, Eq, Data, Typeable )

data FunDep = FunDep [Name] [Name]
  deriving( Show, Eq, Data, Typeable )

data FamFlavour = TypeFam | DataFam
  deriving( Show, Eq, Data, Typeable )

data Foreign = ImportF Callconv Safety String Name Type
             | ExportF Callconv        String Name Type
         deriving( Show, Eq, Data, Typeable )

data Callconv = CCall | StdCall
          deriving( Show, Eq, Data, Typeable )

data Safety = Unsafe | Safe | Interruptible
        deriving( Show, Eq, Data, Typeable )

data Pragma = InlineP         Name Inline RuleMatch Phases
            | SpecialiseP     Name Type (Maybe Inline) Phases
            | SpecialiseInstP Type
            | RuleP           String [RuleBndr] Exp Exp Phases
            | AnnP            AnnTarget Exp
        deriving( Show, Eq, Data, Typeable )

data Inline = NoInline
            | Inline
            | Inlinable
            deriving (Show, Eq, Data, Typeable)

data RuleMatch = ConLike
               | FunLike
               deriving (Show, Eq, Data, Typeable)

data Phases = AllPhases
            | FromPhase Int
            | BeforePhase Int
            deriving (Show, Eq, Data, Typeable)

data RuleBndr = RuleVar Name
              | TypedRuleVar Name Type
              deriving (Show, Eq, Data, Typeable)

data AnnTarget = ModuleAnnotation
               | TypeAnnotation Name
               | ValueAnnotation Name
              deriving (Show, Eq, Data, Typeable)

type Cxt = [Pred]                 -- ^ @(Eq a, Ord b)@

data Pred = ClassP Name [Type]    -- ^ @Eq (Int, a)@
          | EqualP Type Type      -- ^ @F a ~ Bool@
          deriving( Show, Eq, Data, Typeable )

data Strict = IsStrict | NotStrict | Unpacked
         deriving( Show, Eq, Data, Typeable )

data Con = NormalC Name [StrictType]          -- ^ @C Int a@
         | RecC Name [VarStrictType]          -- ^ @C { v :: Int, w :: a }@
         | InfixC StrictType Name StrictType  -- ^ @Int :+ a@
         | ForallC [TyVarBndr] Cxt Con        -- ^ @forall a. Eq a => C [a]@
         deriving( Show, Eq, Data, Typeable )

type StrictType = (Strict, Type)
type VarStrictType = (Name, Strict, Type)

data Type = ForallT [TyVarBndr] Cxt Type  -- ^ @forall \<vars\>. \<ctxt\> -> \<type\>@
          | AppT Type Type                -- ^ @T a b@
          | SigT Type Kind                -- ^ @t :: k@
          | VarT Name                     -- ^ @a@
          | ConT Name                     -- ^ @T@
          | PromotedT Name                -- ^ @'T@

          -- See Note [Representing concrete syntax in types]
          | TupleT Int                    -- ^ @(,), (,,), etc.@
          | UnboxedTupleT Int             -- ^ @(#,#), (#,,#), etc.@
          | ArrowT                        -- ^ @->@
          | ListT                         -- ^ @[]@
          | PromotedTupleT Int            -- ^ @'(), '(,), '(,,), etc.@
          | PromotedNilT                  -- ^ @'[]@
          | PromotedConsT                 -- ^ @(':)@
          | StarT                         -- ^ @*@
          | ConstraintT                   -- ^ @Constraint@
          | LitT TyLit                    -- ^ @0,1,2, etc.@
      deriving( Show, Eq, Data, Typeable )

data TyVarBndr = PlainTV  Name            -- ^ @a@
               | KindedTV Name Kind       -- ^ @(a :: k)@
      deriving( Show, Eq, Data, Typeable )

data TyLit = NumTyLit Integer             -- ^ @2@
           | StrTyLit String              -- ^ @"Hello"@
  deriving ( Show, Eq, Data, Typeable )

-- | Role annotations
data Role = NominalR            -- ^ @nominal@
          | RepresentationalR   -- ^ @representational@
          | PhantomR            -- ^ @phantom@
          | InferR              -- ^ @_@
  deriving( Show, Eq, Data, Typeable )

-- | Annotation target for reifyAnnotations
data AnnLookup = AnnLookupModule Module
               | AnnLookupName Name
               deriving( Show, Eq, Data, Typeable )

-- | To avoid duplication between kinds and types, they
-- are defined to be the same. Naturally, you would never
-- have a type be 'StarT' and you would never have a kind
-- be 'SigT', but many of the other constructors are shared.
-- Note that the kind @Bool@ is denoted with 'ConT', not
-- 'PromotedT'. Similarly, tuple kinds are made with 'TupleT',
-- not 'PromotedTupleT'.

type Kind = Type

{- Note [Representing concrete syntax in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Haskell has a rich concrete syntax for types, including
  t1 -> t2, (t1,t2), [t], and so on
In TH we represent all of this using AppT, with a distinguished
type constructor at the head.  So,
  Type              TH representation
  -----------------------------------------------
  t1 -> t2          ArrowT `AppT` t2 `AppT` t2
  [t]               ListT `AppT` t
  (t1,t2)	    TupleT 2 `AppT` t1 `AppT` t2
  '(t1,t2)          PromotedTupleT 2 `AppT` t1 `AppT` t2

But if the original HsSyn used prefix application, we won't use
these special TH constructors.  For example
  [] t              ConT "[]" `AppT` t
  (->) t            ConT "->" `AppT` t
In this way we can faithfully represent in TH whether the original
HsType used concrete syntax or not.

The one case that doesn't fit this pattern is that of promoted lists
  '[ Maybe, IO ]    PromotedListT 2 `AppT` t1 `AppT` t2
but it's very smelly because there really is no type constructor
corresponding to PromotedListT. So we encode HsExplicitListTy with
PromotedConsT and PromotedNilT (which *do* have underlying type
constructors):
  '[ Maybe, IO ]    PromotedConsT `AppT` Maybe `AppT`
                    (PromotedConsT  `AppT` IO `AppT` PromotedNilT)
-}

-----------------------------------------------------
--		Internal helper functions
-----------------------------------------------------

cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

