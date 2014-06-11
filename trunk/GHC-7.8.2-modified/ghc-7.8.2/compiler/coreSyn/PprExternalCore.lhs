%
% (c) The University of Glasgow 2001-2006
%

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PprExternalCore () where

import Encoding
import ExternalCore

import Pretty
import Data.Char
import Data.Ratio

instance Show Module where
  showsPrec _ m = shows (pmodule m)

instance Show Tdef where
  showsPrec _ t = shows (ptdef t)

instance Show Cdef where
  showsPrec _ c = shows (pcdef c)

instance Show Vdefg where
  showsPrec _ v = shows (pvdefg v)

instance Show Exp where
  showsPrec _ e = shows (pexp e)

instance Show Alt where
  showsPrec _ a = shows (palt a)

instance Show Ty where
  showsPrec _ t = shows (pty t)

instance Show Kind where
  showsPrec _ k = shows (pkind k)

instance Show Lit where
  showsPrec _ l = shows (plit l)


indent :: Doc -> Doc
indent = nest 2

pmodule :: Module -> Doc
pmodule (Module mname tdefs vdefgs) =
  (text "%module" <+> text mname)
    $$ indent ((vcat (map ((<> char ';') . ptdef) tdefs))
               $$ (vcat (map ((<> char ';') . pvdefg) vdefgs)))

ptdef :: Tdef -> Doc
ptdef (Data tcon tbinds cdefs) =
  (text "%data" <+> pqname tcon <+> (hsep (map ptbind tbinds)) <+> char '=')
  $$ indent (braces ((vcat (punctuate (char ';') (map pcdef cdefs)))))

ptdef (Newtype tcon coercion tbinds rep) =
  text "%newtype" <+> pqname tcon <+> pqname coercion 
   <+> (hsep (map ptbind tbinds)) $$ indent repclause
       where repclause = char '=' <+> pty rep

pcdef :: Cdef -> Doc
pcdef (Constr dcon tbinds tys)  =
  (pqname dcon) <+> (sep [hsep (map pattbind tbinds),sep (map paty tys)])
pcdef (GadtConstr dcon ty)  =
  (pqname dcon) <+> text "::" <+> pty ty

pname :: Id -> Doc
pname id = text (zEncodeString id)

pqname :: Qual Id -> Doc
pqname ("",id) = pname id
pqname (m,id)  = text m <> char '.' <> pname id

ptbind, pattbind :: Tbind -> Doc
ptbind (t,Klifted) = pname t
ptbind (t,k) = parens (pname t <> text "::" <> pkind k)

pattbind (t,k) = char '@' <> ptbind (t,k)

pakind, pkind :: Kind -> Doc
pakind (Klifted) = char '*'
pakind (Kunlifted) = char '#'
pakind (Kopen) = char '?'
pakind k = parens (pkind k)

pkind (Karrow k1 k2) = parens (pakind k1 <> text "->" <> pkind k2)
pkind k = pakind k

paty, pbty, pty :: Ty -> Doc
-- paty: print in parens, if non-atomic (like a name)
-- pbty: print in parens, if arrow (used only for lhs of arrow)
-- pty:  not in parens
paty (Tvar n) = pname n
paty (Tcon c) = pqname c
paty t = parens (pty t)

pbty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = parens(fsep [pbty t1, text "->",pty t2])
pbty t = paty t

pty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = fsep [pbty t1, text "->",pty t2]
pty (Tforall tb t) = text "%forall" <+> pforall [tb] t
pty ty@(Tapp {}) = pappty ty []
pty ty@(Tvar {}) = paty ty
pty ty@(Tcon {}) = paty ty

pappty :: Ty -> [Ty] -> Doc
pappty (Tapp t1 t2) ts = pappty t1 (t2:ts)
pappty t ts = sep (map paty (t:ts))

pforall :: [Tbind] -> Ty -> Doc
pforall tbs (Tforall tb t) = pforall (tbs ++ [tb]) t
pforall tbs t = hsep (map ptbind tbs) <+> char '.' <+> pty t

paco, pbco, pco :: Coercion -> Doc
paco (ReflCoercion r ty) = char '<' <> pty ty <> text ">_" <> prole r
paco (TyConAppCoercion r qtc []) = pqname qtc <> char '_' <> prole r
paco (AxiomCoercion qtc i []) = pqname qtc <> char '[' <> int i <> char ']'
paco (CoVarCoercion cv) = pname cv
paco c = parens (pco c)

pbco (TyConAppCoercion _ arr [co1, co2])
  | arr == tcArrow
  = parens (fsep [pbco co1, text "->", pco co2])
pbco co = paco co

pco c@(ReflCoercion {})          = paco c
pco (SymCoercion co)             = sep [text "%sub", paco co]
pco (TransCoercion co1 co2)      = sep [text "%trans", paco co1, paco co2]
pco (TyConAppCoercion _ arr [co1, co2])
  | arr == tcArrow               = fsep [pbco co1, text "->", pco co2]
pco (TyConAppCoercion r qtc cos) = parens (pqname qtc <+> sep (map paco cos)) <> char '_' <> prole r
pco co@(AppCoercion {})          = pappco co []
pco (ForAllCoercion tb co)       = text "%forall" <+> pforallco [tb] co
pco co@(CoVarCoercion {})        = paco co
pco (UnivCoercion r ty1 ty2)     = sep [text "%univ", prole r, paty ty1, paty ty2]
pco (InstCoercion co ty)         = sep [text "%inst", paco co, paty ty]
pco (NthCoercion i co)           = sep [text "%nth", int i, paco co]
pco (AxiomCoercion qtc i cos)    = pqname qtc <> char '[' <> int i <> char ']' <+> sep (map paco cos)
pco (LRCoercion CLeft co)        = sep [text "%left", paco co]
pco (LRCoercion CRight co)       = sep [text "%right", paco co]
pco (SubCoercion co)             = sep [text "%sub", paco co]

pappco :: Coercion -> [Coercion ] -> Doc
pappco (AppCoercion co1 co2) cos = pappco co1 (co2:cos)
pappco co cos = sep (map paco (co:cos))

pforallco :: [Tbind] -> Coercion -> Doc
pforallco tbs (ForAllCoercion tb co) = pforallco (tbs ++ [tb]) co
pforallco tbs co = hsep (map ptbind tbs) <+> char '.' <+> pco co
 
prole :: Role -> Doc
prole Nominal          = char 'N'
prole Representational = char 'R'
prole Phantom          = char 'P'

pvdefg :: Vdefg -> Doc
pvdefg (Rec vdefs) = text "%rec" $$ braces (indent (vcat (punctuate (char ';') (map pvdef vdefs))))
pvdefg (Nonrec vdef) = pvdef vdef

pvdef :: Vdef -> Doc
-- TODO: Think about whether %local annotations are actually needed.
-- Right now, the local flag is never used, because the Core doc doesn't
-- explain the meaning of %local.
pvdef (_l,v,t,e) = sep [(pqname v <+> text "::" <+> pty t <+> char '='),
                    indent (pexp e)]

paexp, pfexp, pexp :: Exp -> Doc
paexp (Var x) = pqname x
paexp (Dcon x) = pqname x
paexp (Lit l) = plit l
paexp e = parens(pexp e)

plamexp :: [Bind] -> Exp -> Doc
plamexp bs (Lam b e) = plamexp (bs ++ [b]) e
plamexp bs e = sep [sep (map pbind bs) <+> text "->",
                    indent (pexp e)]

pbind :: Bind -> Doc
pbind (Tb tb) = char '@' <+> ptbind tb
pbind (Vb vb) = pvbind vb

pfexp (App e1 e2) = pappexp e1 [Left e2]
pfexp (Appt e t) = pappexp e [Right t]
pfexp e = paexp e

pappexp :: Exp -> [Either Exp Ty] -> Doc
pappexp (App e1 e2) as = pappexp e1 (Left e2:as)
pappexp (Appt e t) as = pappexp e (Right t:as)
pappexp e as = fsep (paexp e : map pa as)
           where pa (Left e) = paexp e
                 pa (Right t) = char '@' <+> paty t

pexp (Lam b e) = char '\\' <+> plamexp [b] e
pexp (Let vd e) = (text "%let" <+> pvdefg vd) $$ (text "%in" <+> pexp e)
pexp (Case e vb ty alts) = sep [text "%case" <+> paty ty <+> paexp e,
                             text "%of" <+> pvbind vb]
                        $$ (indent (braces (vcat (punctuate (char ';') (map palt alts)))))
pexp (Cast e co) = (text "%cast" <+> parens (pexp e)) $$ paco co
pexp (Tick s e) = (text "%source" <+> pstring s) $$ pexp e
pexp (External n cc t) = (text "%external" <+> text cc <+> pstring n) $$ paty t
pexp (DynExternal cc t) = (text "%dynexternal" <+> text cc) $$ paty t
pexp (Label n) = (text "%label" <+> pstring n)
pexp e = pfexp e

pvbind :: Vbind -> Doc
pvbind (x,t) = parens(pname x <> text "::" <> pty t)

palt :: Alt -> Doc
palt (Acon c tbs vbs e) =
        sep [pqname c, 
             sep (map pattbind tbs),
             sep (map pvbind vbs) <+> text "->"]
        $$ indent (pexp e)
palt (Alit l e) = 
        (plit l <+>  text "->")
        $$ indent (pexp e)
palt (Adefault e) = 
        (text "%_ ->")
        $$ indent (pexp e)

plit :: Lit -> Doc
plit (Lint i t) = parens (integer i <> text "::" <> pty t)
-- we use (text (show (numerator r))) (and the same for denominator)
-- because "(rational r)" was printing out things like "2.0e-2" (which
-- isn't External Core), and (text (show r)) was printing out things
-- like "((-1)/5)" which isn't either (it should be "(-1/5)").
plit (Lrational r t) = parens (text (show (numerator r)) <+> char '%'
   <+> text (show (denominator r)) <>  text "::" <> pty t)
plit (Lchar c t) = parens (text ("\'" ++ escape [c] ++ "\'") <> text "::" <> pty t)
-- This is a little messy. We shouldn't really be going via String.
plit (Lstring bs t) = parens (pstring str <> text "::" <> pty t)
    where str = map (chr . fromIntegral) bs

pstring :: String -> Doc
pstring s = doubleQuotes(text (escape s))

escape :: String -> String
escape s = foldr f [] (map ord s)
    where 
     f cv rest
        | cv > 0xFF = '\\':'x':hs ++ rest
        | (cv < 0x20 || cv > 0x7e || cv == 0x22 || cv == 0x27 || cv == 0x5c) = 
         '\\':'x':h1:h0:rest
           where (q1,r1) = quotRem cv 16
                 h1 = intToDigit q1
                 h0 = intToDigit r1
                 hs = dropWhile (=='0') $ reverse $ mkHex cv
                 mkHex 0 = ""
                 mkHex cv = intToDigit r : mkHex q
                    where (q,r) = quotRem cv 16
     f cv rest = (chr cv):rest

\end{code}




