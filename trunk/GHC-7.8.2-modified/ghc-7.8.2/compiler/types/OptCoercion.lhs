%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module OptCoercion ( optCoercion, checkAxInstCo ) where 

#include "HsVersions.h"

import Coercion
import Type hiding( substTyVarBndr, substTy, extendTvSubst )
import TcType       ( exactTyVarsOfType )
import TyCon
import CoAxiom
import Var
import VarSet
import FamInstEnv   ( flattenTys )
import VarEnv
import StaticFlags	( opt_NoOptCoercion )
import Outputable
import Pair
import Maybes
import FastString
import Util
import Unify
import ListSetOps
import InstEnv
\end{code}

%************************************************************************
%*                                                                      *
                 Optimising coercions									
%*                                                                      *
%************************************************************************

Note [Subtle shadowing in coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose we optimising a coercion
    optCoercion (forall (co_X5:t1~t2). ...co_B1...)
The co_X5 is a wild-card; the bound variable of a coercion for-all
should never appear in the body of the forall. Indeed we often
write it like this
    optCoercion ( (t1~t2) => ...co_B1... )

Just because it's a wild-card doesn't mean we are free to choose
whatever variable we like.  For example it'd be wrong for optCoercion
to return
   forall (co_B1:t1~t2). ...co_B1...
because now the co_B1 (which is really free) has been captured, and
subsequent substitutions will go wrong.  That's why we can't use
mkCoPredTy in the ForAll case, where this note appears.  

\begin{code}
optCoercion :: CvSubst -> Coercion -> NormalCo
-- ^ optCoercion applies a substitution to a coercion, 
--   *and* optimises it to reduce its size
optCoercion env co 
  | opt_NoOptCoercion = substCo env co
  | otherwise         = opt_co env False Nothing co

type NormalCo = Coercion
  -- Invariants: 
  --  * The substitution has been fully applied
  --  * For trans coercions (co1 `trans` co2)
  --       co1 is not a trans, and neither co1 nor co2 is identity
  --  * If the coercion is the identity, it has no CoVars of CoTyCons in it (just types)

type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity

opt_co, opt_co' :: CvSubst
       		-> Bool	       -- True <=> return (sym co)
                -> Maybe Role  -- Nothing <=> don't change; otherwise, change
                               -- INVARIANT: the change is always a *downgrade*
       		-> Coercion
       		-> NormalCo	
opt_co = opt_co' 
{-
opt_co env sym co
 = pprTrace "opt_co {" (ppr sym <+> ppr co $$ ppr env) $
   co1 `seq`
   pprTrace "opt_co done }" (ppr co1) $
   (WARN( not same_co_kind, ppr co  <+> dcolon <+> pprEqPred (Pair s1 t1)
                         $$ ppr co1 <+> dcolon <+> pprEqPred (Pair s2 t2) )
    WARN( not (coreEqCoercion co1 simple_result),
           (text "env=" <+> ppr env) $$
           (text "input=" <+> ppr co) $$
           (text "simple=" <+> ppr simple_result) $$
           (text "opt=" <+> ppr co1) )
   co1)
 where
   co1 = opt_co' env sym co
   same_co_kind = s1 `eqType` s2 && t1 `eqType` t2
   Pair s t = coercionKind (substCo env co)
   (s1,t1) | sym = (t,s)
           | otherwise = (s,t)
   Pair s2 t2 = coercionKind co1

   simple_result | sym = mkSymCo (substCo env co)
                 | otherwise = substCo env co
-}

opt_co' env _   mrole (Refl r ty) = Refl (mrole `orElse` r) (substTy env ty)
opt_co' env sym mrole co
  |  mrole == Just Phantom 
  || coercionRole co == Phantom
  , Pair ty1 ty2 <- coercionKind co
  = if sym
    then opt_univ env Phantom ty2 ty1
    else opt_univ env Phantom ty1 ty2

opt_co' env sym mrole (SymCo co)  = opt_co env (not sym) mrole co
opt_co' env sym mrole (TyConAppCo r tc cos)
  = case mrole of
      Nothing -> mkTyConAppCo r  tc (map (opt_co env sym Nothing) cos)
      Just r' -> mkTyConAppCo r' tc (zipWith (opt_co env sym)
                                             (map Just (tyConRolesX r' tc)) cos)
opt_co' env sym mrole (AppCo co1 co2) = mkAppCo (opt_co env sym mrole   co1)
                                                (opt_co env sym Nothing co2)
opt_co' env sym mrole (ForAllCo tv co)
  = case substTyVarBndr env tv of
      (env', tv') -> mkForAllCo tv' (opt_co env' sym mrole co)
     -- Use the "mk" functions to check for nested Refls

opt_co' env sym mrole (CoVarCo cv)
  | Just co <- lookupCoVar env cv
  = opt_co (zapCvSubstEnv env) sym mrole co

  | Just cv1 <- lookupInScope (getCvInScope env) cv
  = ASSERT( isCoVar cv1 ) wrapRole mrole cv_role $ wrapSym sym (CoVarCo cv1)
                -- cv1 might have a substituted kind!

  | otherwise = WARN( True, ptext (sLit "opt_co: not in scope:") <+> ppr cv $$ ppr env)
                ASSERT( isCoVar cv )
                wrapRole mrole cv_role $ wrapSym sym (CoVarCo cv)
  where cv_role = coVarRole cv

opt_co' env sym mrole (AxiomInstCo con ind cos)
    -- Do *not* push sym inside top-level axioms
    -- e.g. if g is a top-level axiom
    --   g a : f a ~ a
    -- then (sym (g ty)) /= g (sym ty) !!
  = wrapRole mrole (coAxiomRole con) $
    wrapSym sym $
    AxiomInstCo con ind (map (opt_co env False Nothing) cos)
      -- Note that the_co does *not* have sym pushed into it

opt_co' env sym mrole (UnivCo r oty1 oty2)
  = opt_univ env role a b
  where
    (a,b) = if sym then (oty2,oty1) else (oty1,oty2)
    role = mrole `orElse` r

opt_co' env sym mrole (TransCo co1 co2)
  | sym       = opt_trans in_scope opt_co2 opt_co1   -- sym (g `o` h) = sym h `o` sym g
  | otherwise = opt_trans in_scope opt_co1 opt_co2
  where
    opt_co1 = opt_co env sym mrole co1
    opt_co2 = opt_co env sym mrole co2
    in_scope = getCvInScope env

-- NthCo roles are fiddly!
opt_co' env sym mrole (NthCo n (TyConAppCo _ _ cos))
  = opt_co env sym mrole (getNth cos n)
opt_co' env sym mrole (NthCo n co)
  | TyConAppCo _ _tc cos <- co'
  , isDecomposableTyCon tc   -- Not synonym families
  = ASSERT( n < length cos )
    ASSERT( _tc == tc )
    let resultCo = cos !! n
        resultRole = coercionRole resultCo in
    case (mrole, resultRole) of
        -- if we just need an R coercion, try to propagate the SubCo again:
      (Just Representational, Nominal) -> opt_co (zapCvSubstEnv env) False mrole resultCo
      _                                -> resultCo

  | otherwise
  = wrap_role $ NthCo n co'

  where
    wrap_role wrapped = wrapRole mrole (coercionRole wrapped) wrapped

    tc = tyConAppTyCon $ pFst $ coercionKind co
    co' = opt_co env sym mrole' co
    mrole' = case mrole of
               Just Representational
                 | Representational <- nthRole Representational tc n
                 -> Just Representational
               _ -> Nothing

opt_co' env sym mrole (LRCo lr co)
  | Just pr_co <- splitAppCo_maybe co
  = opt_co env sym mrole (pickLR lr pr_co)
  | Just pr_co <- splitAppCo_maybe co'
  = if mrole == Just Representational
    then opt_co (zapCvSubstEnv env) False mrole (pickLR lr pr_co)
    else pickLR lr pr_co
  | otherwise
  = wrapRole mrole Nominal $ LRCo lr co'
  where
    co' = opt_co env sym Nothing co

opt_co' env sym mrole (InstCo co ty)
    -- See if the first arg is already a forall
    -- ...then we can just extend the current substitution
  | Just (tv, co_body) <- splitForAllCo_maybe co
  = opt_co (extendTvSubst env tv ty') sym mrole co_body

     -- See if it is a forall after optimization
     -- If so, do an inefficient one-variable substitution
  | Just (tv, co'_body) <- splitForAllCo_maybe co'
  = substCoWithTy (getCvInScope env) tv ty' co'_body   

  | otherwise = InstCo co' ty'
  where
    co' = opt_co env sym mrole co
    ty' = substTy env ty

opt_co' env sym _ (SubCo co) = opt_co env sym (Just Representational) co

-- XXX: We could add another field to CoAxiomRule that
-- would allow us to do custom simplifications.
opt_co' env sym mrole (AxiomRuleCo co ts cs) =
  wrapRole mrole (coaxrRole co) $
    wrapSym sym $
    AxiomRuleCo co (map (substTy env) ts)
                   (zipWith (opt_co env False) (map Just (coaxrAsmpRoles co)) cs)



-------------
opt_univ :: CvSubst -> Role -> Type -> Type -> Coercion
opt_univ env role oty1 oty2
  | Just (tc1, tys1) <- splitTyConApp_maybe oty1
  , Just (tc2, tys2) <- splitTyConApp_maybe oty2
  , tc1 == tc2
  = mkTyConAppCo role tc1 (zipWith3 (opt_univ env) (tyConRolesX role tc1) tys1 tys2)

  | Just (l1, r1) <- splitAppTy_maybe oty1
  , Just (l2, r2) <- splitAppTy_maybe oty2
  , typeKind l1 `eqType` typeKind l2   -- kind(r1) == kind(r2) by consequence
  = let role' = if role == Phantom then Phantom else Nominal in
       -- role' is to comform to mkAppCo's precondition
    mkAppCo (opt_univ env role l1 l2) (opt_univ env role' r1 r2)

  | Just (tv1, ty1) <- splitForAllTy_maybe oty1
  , Just (tv2, ty2) <- splitForAllTy_maybe oty2
  , tyVarKind tv1 `eqType` tyVarKind tv2  -- rule out a weird unsafeCo
  = case substTyVarBndr2 env tv1 tv2 of { (env1, env2, tv') ->
    let ty1' = substTy env1 ty1
        ty2' = substTy env2 ty2 in
    mkForAllCo tv' (opt_univ (zapCvSubstEnv2 env1 env2) role ty1' ty2') }

  | otherwise
  = mkUnivCo role (substTy env oty1) (substTy env oty2)

-------------
opt_transList :: InScopeSet -> [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transList is = zipWith (opt_trans is)

opt_trans :: InScopeSet -> NormalCo -> NormalCo -> NormalCo
opt_trans is co1 co2
  | isReflCo co1 = co2
  | otherwise    = opt_trans1 is co1 co2

opt_trans1 :: InScopeSet -> NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 is co1 co2
  | isReflCo co2 = co1
  | otherwise    = opt_trans2 is co1 co2

opt_trans2 :: InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> NormalCo
-- Neither arg is the identity
opt_trans2 is (TransCo co1a co1b) co2
    -- Don't know whether the sub-coercions are the identity
  = opt_trans is co1a (opt_trans is co1b co2)  

opt_trans2 is co1 co2 
  | Just co <- opt_trans_rule is co1 co2
  = co

opt_trans2 is co1 (TransCo co2a co2b)
  | Just co1_2a <- opt_trans_rule is co1 co2a
  = if isReflCo co1_2a
    then co2b
    else opt_trans1 is co1_2a co2b

opt_trans2 _ co1 co2
  = mkTransCo co1 co2

------
-- Optimize coercions with a top-level use of transitivity.
opt_trans_rule :: InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo

-- Push transitivity through matching destructors
opt_trans_rule is in_co1@(NthCo d1 co1) in_co2@(NthCo d2 co2)
  | d1 == d2
  , co1 `compatible_co` co2
  = fireTransRule "PushNth" in_co1 in_co2 $
    mkNthCo d1 (opt_trans is co1 co2)

opt_trans_rule is in_co1@(LRCo d1 co1) in_co2@(LRCo d2 co2)
  | d1 == d2
  , co1 `compatible_co` co2
  = fireTransRule "PushLR" in_co1 in_co2 $
    mkLRCo d1 (opt_trans is co1 co2)

-- Push transitivity inside instantiation
opt_trans_rule is in_co1@(InstCo co1 ty1) in_co2@(InstCo co2 ty2)
  | ty1 `eqType` ty2
  , co1 `compatible_co` co2
  = fireTransRule "TrPushInst" in_co1 in_co2 $
    mkInstCo (opt_trans is co1 co2) ty1
 
-- Push transitivity down through matching top-level constructors.
opt_trans_rule is in_co1@(TyConAppCo r1 tc1 cos1) in_co2@(TyConAppCo r2 tc2 cos2)
  | tc1 == tc2 
  = ASSERT( r1 == r2 )
    fireTransRule "PushTyConApp" in_co1 in_co2 $
    TyConAppCo r1 tc1 (opt_transList is cos1 cos2)

opt_trans_rule is in_co1@(AppCo co1a co1b) in_co2@(AppCo co2a co2b)
  = fireTransRule "TrPushApp" in_co1 in_co2 $
    mkAppCo (opt_trans is co1a co2a) (opt_trans is co1b co2b)

-- Eta rules
opt_trans_rule is co1@(TyConAppCo r tc cos1) co2
  | Just cos2 <- etaTyConAppCo_maybe tc co2
  = ASSERT( length cos1 == length cos2 )
    fireTransRule "EtaCompL" co1 co2 $
    TyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1 co2@(TyConAppCo r tc cos2)
  | Just cos1 <- etaTyConAppCo_maybe tc co1
  = ASSERT( length cos1 == length cos2 )
    fireTransRule "EtaCompR" co1 co2 $
    TyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1@(AppCo co1a co1b) co2
  | Just (co2a,co2b) <- etaAppCo_maybe co2
  = fireTransRule "EtaAppL" co1 co2 $
    mkAppCo (opt_trans is co1a co2a) (opt_trans is co1b co2b)

opt_trans_rule is co1 co2@(AppCo co2a co2b)
  | Just (co1a,co1b) <- etaAppCo_maybe co1
  = fireTransRule "EtaAppR" co1 co2 $
    mkAppCo (opt_trans is co1a co2a) (opt_trans is co1b co2b)

-- Push transitivity inside forall
opt_trans_rule is co1 co2
  | Just (tv1,r1) <- splitForAllCo_maybe co1
  , Just (tv2,r2) <- etaForAllCo_maybe co2
  , let r2' = substCoWithTy is' tv2 (mkTyVarTy tv1) r2
        is' = is `extendInScopeSet` tv1
  = fireTransRule "EtaAllL" co1 co2 $
    mkForAllCo tv1 (opt_trans2 is' r1 r2')

  | Just (tv2,r2) <- splitForAllCo_maybe co2
  , Just (tv1,r1) <- etaForAllCo_maybe co1
  , let r1' = substCoWithTy is' tv1 (mkTyVarTy tv2) r1
        is' = is `extendInScopeSet` tv2
  = fireTransRule "EtaAllR" co1 co2 $
    mkForAllCo tv1 (opt_trans2 is' r1' r2)

-- Push transitivity inside axioms
opt_trans_rule is co1 co2

  -- TrPushSymAxR
  | Just (sym, con, ind, cos1) <- co1_is_axiom_maybe
  , Just cos2 <- matchAxiom sym con ind co2
  , True <- sym
  , let newAxInst = AxiomInstCo con ind (opt_transList is (map mkSymCo cos2) cos1)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushSymAxR" co1 co2 $ SymCo newAxInst

  -- TrPushAxR
  | Just (sym, con, ind, cos1) <- co1_is_axiom_maybe
  , Just cos2 <- matchAxiom sym con ind co2
  , False <- sym
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos1 cos2)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushAxR" co1 co2 newAxInst

  -- TrPushSymAxL
  | Just (sym, con, ind, cos2) <- co2_is_axiom_maybe
  , Just cos1 <- matchAxiom (not sym) con ind co1
  , True <- sym
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos2 (map mkSymCo cos1))
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushSymAxL" co1 co2 $ SymCo newAxInst

  -- TrPushAxL  
  | Just (sym, con, ind, cos2) <- co2_is_axiom_maybe
  , Just cos1 <- matchAxiom (not sym) con ind co1
  , False <- sym
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos1 cos2)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushAxL" co1 co2 newAxInst

  -- TrPushAxSym/TrPushSymAx
  | Just (sym1, con1, ind1, cos1) <- co1_is_axiom_maybe
  , Just (sym2, con2, ind2, cos2) <- co2_is_axiom_maybe
  , con1 == con2
  , ind1 == ind2
  , sym1 == not sym2
  , let branch = coAxiomNthBranch con1 ind1
        qtvs = coAxBranchTyVars branch
        lhs  = coAxNthLHS con1 ind1
        rhs  = coAxBranchRHS branch
        pivot_tvs = exactTyVarsOfType (if sym2 then rhs else lhs)
  , all (`elemVarSet` pivot_tvs) qtvs
  = fireTransRule "TrPushAxSym" co1 co2 $
    if sym2
    then liftCoSubstWith role qtvs (opt_transList is cos1 (map mkSymCo cos2)) lhs  -- TrPushAxSym
    else liftCoSubstWith role qtvs (opt_transList is (map mkSymCo cos1) cos2) rhs  -- TrPushSymAx
  where
    co1_is_axiom_maybe = isAxiom_maybe co1
    co2_is_axiom_maybe = isAxiom_maybe co2
    role = coercionRole co1 -- should be the same as coercionRole co2!

opt_trans_rule _ co1 co2	-- Identity rule
  | Pair ty1 _ <- coercionKind co1
  , Pair _ ty2 <- coercionKind co2
  , ty1 `eqType` ty2
  = fireTransRule "RedTypeDirRefl" co1 co2 $
    Refl (coercionRole co1) ty2

opt_trans_rule _ _ _ = Nothing

fireTransRule :: String -> Coercion -> Coercion -> Coercion -> Maybe Coercion
fireTransRule _rule _co1 _co2 res
  = -- pprTrace ("Trans rule fired: " ++ _rule) (vcat [ppr _co1, ppr _co2, ppr res]) $
    Just res

\end{code}

Note [Conflict checking with AxiomInstCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following type family and axiom:

type family Equal (a :: k) (b :: k) :: Bool
type instance where
  Equal a a = True
  Equal a b = False
--
Equal :: forall k::BOX. k -> k -> Bool
axEqual :: { forall k::BOX. forall a::k. Equal k a a ~ True
           ; forall k::BOX. forall a::k. forall b::k. Equal k a b ~ False }

We wish to disallow (axEqual[1] <*> <Int> <Int). (Recall that the index is 0-based,
so this is the second branch of the axiom.) The problem is that, on the surface, it
seems that (axEqual[1] <*> <Int> <Int>) :: (Equal * Int Int ~ False) and that all is
OK. But, all is not OK: we want to use the first branch of the axiom in this case,
not the second. The problem is that the parameters of the first branch can unify with
the supplied coercions, thus meaning that the first branch should be taken. See also
Note [Branched instance checking] in types/FamInstEnv.lhs.

\begin{code}
-- | Check to make sure that an AxInstCo is internally consistent.
-- Returns the conflicting branch, if it exists
-- See Note [Conflict checking with AxiomInstCo]
checkAxInstCo :: Coercion -> Maybe CoAxBranch
-- defined here to avoid dependencies in Coercion
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in CoreLint
checkAxInstCo (AxiomInstCo ax ind cos)
  = let branch = coAxiomNthBranch ax ind
        tvs = coAxBranchTyVars branch
        incomps = coAxBranchIncomps branch
        tys = map (pFst . coercionKind) cos 
        subst = zipOpenTvSubst tvs tys
        target = Type.substTys subst (coAxBranchLHS branch)
        in_scope = mkInScopeSet $
                   unionVarSets (map (tyVarsOfTypes . coAxBranchLHS) incomps)
        flattened_target = flattenTys in_scope target in
    check_no_conflict flattened_target incomps
  where
    check_no_conflict :: [Type] -> [CoAxBranch] -> Maybe CoAxBranch
    check_no_conflict _    [] = Nothing
    check_no_conflict flat (b@CoAxBranch { cab_lhs = lhs_incomp } : rest)
         -- See Note [Apartness] in FamInstEnv
      | SurelyApart <- tcUnifyTysFG instanceBindFun flat lhs_incomp
      = check_no_conflict flat rest
      | otherwise
      = Just b
checkAxInstCo _ = Nothing

-----------
wrapSym :: Bool -> Coercion -> Coercion
wrapSym sym co | sym       = SymCo co
               | otherwise = co

wrapRole :: Maybe Role   -- desired
         -> Role         -- current
         -> Coercion -> Coercion
wrapRole Nothing        _       = id
wrapRole (Just desired) current = maybeSubCo2 desired current

-----------
-- takes two tyvars and builds env'ts to map them to the same tyvar
substTyVarBndr2 :: CvSubst -> TyVar -> TyVar
                -> (CvSubst, CvSubst, TyVar)
substTyVarBndr2 env tv1 tv2
  = case substTyVarBndr env tv1 of
      (env1, tv1') -> (env1, extendTvSubstAndInScope env tv2 (mkTyVarTy tv1'), tv1')
    
zapCvSubstEnv2 :: CvSubst -> CvSubst -> CvSubst
zapCvSubstEnv2 env1 env2 = mkCvSubst (is1 `unionInScope` is2) []
  where is1 = getCvInScope env1
        is2 = getCvInScope env2
-----------
isAxiom_maybe :: Coercion -> Maybe (Bool, CoAxiom Branched, Int, [Coercion])
isAxiom_maybe (SymCo co) 
  | Just (sym, con, ind, cos) <- isAxiom_maybe co
  = Just (not sym, con, ind, cos)
isAxiom_maybe (AxiomInstCo con ind cos)
  = Just (False, con, ind, cos)
isAxiom_maybe _ = Nothing

matchAxiom :: Bool -- True = match LHS, False = match RHS
           -> CoAxiom br -> Int -> Coercion -> Maybe [Coercion]
-- If we succeed in matching, then *all the quantified type variables are bound*
-- E.g.   if tvs = [a,b], lhs/rhs = [b], we'll fail
matchAxiom sym ax@(CoAxiom { co_ax_tc = tc }) ind co
  = let (CoAxBranch { cab_tvs   = qtvs
                    , cab_roles = roles
                    , cab_lhs   = lhs
                    , cab_rhs   = rhs }) = coAxiomNthBranch ax ind in
    case liftCoMatch (mkVarSet qtvs) (if sym then (mkTyConApp tc lhs) else rhs) co of
      Nothing    -> Nothing
      Just subst -> allMaybes (zipWith (liftCoSubstTyVar subst) roles qtvs)

-------------
compatible_co :: Coercion -> Coercion -> Bool
-- Check whether (co1 . co2) will be well-kinded
compatible_co co1 co2
  = x1 `eqType` x2		
  where
    Pair _ x1 = coercionKind co1
    Pair x2 _ = coercionKind co2

-------------
etaForAllCo_maybe :: Coercion -> Maybe (TyVar, Coercion)
-- Try to make the coercion be of form (forall tv. co)
etaForAllCo_maybe co
  | Just (tv, r) <- splitForAllCo_maybe co
  = Just (tv, r)

  | Pair ty1 ty2  <- coercionKind co
  , Just (tv1, _) <- splitForAllTy_maybe ty1
  , Just (tv2, _) <- splitForAllTy_maybe ty2
  , tyVarKind tv1 `eqKind` tyVarKind tv2
  = Just (tv1, mkInstCo co (mkTyVarTy tv1))

  | otherwise
  = Nothing

etaAppCo_maybe :: Coercion -> Maybe (Coercion,Coercion)
-- If possible, split a coercion
--   g :: t1a t1b ~ t2a t2b
-- into a pair of coercions (left g, right g)
etaAppCo_maybe co
  | Just (co1,co2) <- splitAppCo_maybe co
  = Just (co1,co2)
  | Nominal <- coercionRole co
  , Pair ty1 ty2 <- coercionKind co
  , Just (_,t1) <- splitAppTy_maybe ty1
  , Just (_,t2) <- splitAppTy_maybe ty2
  , typeKind t1 `eqType` typeKind t2      -- Note [Eta for AppCo]
  = Just (LRCo CLeft co, LRCo CRight co)
  | otherwise
  = Nothing

etaTyConAppCo_maybe :: TyCon -> Coercion -> Maybe [Coercion]
-- If possible, split a coercion 
--       g :: T s1 .. sn ~ T t1 .. tn
-- into [ Nth 0 g :: s1~t1, ..., Nth (n-1) g :: sn~tn ] 
etaTyConAppCo_maybe tc (TyConAppCo _ tc2 cos2)
  = ASSERT( tc == tc2 ) Just cos2

etaTyConAppCo_maybe tc co
  | isDecomposableTyCon tc
  , Pair ty1 ty2     <- coercionKind co
  , Just (tc1, tys1) <- splitTyConApp_maybe ty1
  , Just (tc2, tys2) <- splitTyConApp_maybe ty2
  , tc1 == tc2
  , let n = length tys1
  = ASSERT( tc == tc1 ) 
    ASSERT( n == length tys2 )
    Just (decomposeCo n co)
    -- NB: n might be <> tyConArity tc
    -- e.g.   data family T a :: * -> *
    --        g :: T a b ~ T c d

  | otherwise
  = Nothing
\end{code}  

Note [Eta for AppCo]
~~~~~~~~~~~~~~~~~~~~
Suppose we have 
   g :: s1 t1 ~ s2 t2

Then we can't necessarily make
   left  g :: s1 ~ s2
   right g :: t1 ~ t2
because it's possible that
   s1 :: * -> *         t1 :: *
   s2 :: (*->*) -> *    t2 :: * -> *
and in that case (left g) does not have the same
kind on either side.

It's enough to check that 
  kind t1 = kind t2
because if g is well-kinded then
  kind (s1 t2) = kind (s2 t2)
and these two imply
  kind s1 = kind s2

