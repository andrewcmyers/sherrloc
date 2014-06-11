{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module CmmParse ( parseCmmFile ) where

import StgCmmExtCode
import CmmCallConv
import StgCmmProf
import StgCmmHeap
import StgCmmMonad hiding ( getCode, getCodeR, emitLabel, emit, emitStore
                          , emitAssign, emitOutOfLine, withUpdFrameOff
                          , getUpdFrameOff )
import qualified StgCmmMonad as F
import StgCmmUtils
import StgCmmForeign
import StgCmmExpr
import StgCmmClosure
import StgCmmLayout     hiding (ArgRep(..))
import StgCmmTicky
import StgCmmBind       ( emitBlackHoleCode, emitUpdateFrame )

import CmmOpt
import MkGraph
import Cmm
import CmmUtils
import CmmInfo
import BlockId
import CmmLex
import CLabel
import SMRep
import Lexer

import CostCentre
import ForeignCall
import Module
import Platform
import Literal
import Unique
import UniqFM
import SrcLoc
import DynFlags
import StaticFlags
import ErrUtils
import StringBuffer
import FastString
import Panic
import Constants
import Outputable
import BasicTypes
import Bag              ( emptyBag, unitBag )
import Var

import Control.Monad
import Data.Array
import Data.Char        ( ord )
import System.Exit
import Data.Maybe

#include "HsVersions.h"
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.19.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (CmmParse CLabel) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (CmmParse CLabel)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([CmmParse [CmmStatic]]) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([CmmParse [CmmStatic]])
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (CmmParse [CmmStatic]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (CmmParse [CmmStatic])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([CmmParse CmmExpr]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([CmmParse CmmExpr])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Convention) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Convention)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (CmmParse (CLabel, Maybe CmmInfoTable, [LocalReg])) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (CmmParse (CLabel, Maybe CmmInfoTable, [LocalReg]))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ([(FastString, CLabel)]) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ([(FastString, CLabel)])
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ((FastString,  CLabel)) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ((FastString,  CLabel))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([FastString]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([FastString])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (CmmParse CmmExpr) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (CmmParse CmmExpr)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (CmmReturnInfo) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (CmmReturnInfo)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (CmmParse BoolExpr) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (CmmParse BoolExpr)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (CmmParse BoolExpr) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (CmmParse BoolExpr)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Safety) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Safety)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ([GlobalReg]) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ([GlobalReg])
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([GlobalReg]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([GlobalReg])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Maybe (Int,Int)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Maybe (Int,Int))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([CmmParse ([Int],Either BlockId (CmmParse ()))]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([CmmParse ([Int],Either BlockId (CmmParse ()))])
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (CmmParse ([Int],Either BlockId (CmmParse ()))) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (CmmParse ([Int],Either BlockId (CmmParse ())))
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (CmmParse (Either BlockId (CmmParse ()))) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (CmmParse (Either BlockId (CmmParse ())))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([Int]) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([Int])
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Maybe (CmmParse ())) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Maybe (CmmParse ()))
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (CmmParse ()) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (CmmParse ())
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (CmmParse CmmExpr) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (CmmParse CmmExpr)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (CmmParse CmmExpr) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (CmmParse CmmExpr)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (CmmType) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (CmmType)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([CmmParse (CmmExpr, ForeignHint)]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([CmmParse (CmmExpr, ForeignHint)])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([CmmParse (CmmExpr, ForeignHint)]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([CmmParse (CmmExpr, ForeignHint)])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (CmmParse (CmmExpr, ForeignHint)) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (CmmParse (CmmExpr, ForeignHint))
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([CmmParse CmmExpr]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([CmmParse CmmExpr])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([CmmParse CmmExpr]) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ([CmmParse CmmExpr])
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (CmmParse CmmExpr) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (CmmParse CmmExpr)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([CmmParse (LocalReg, ForeignHint)]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([CmmParse (LocalReg, ForeignHint)])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([CmmParse (LocalReg, ForeignHint)]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([CmmParse (LocalReg, ForeignHint)])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (CmmParse (LocalReg, ForeignHint)) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (CmmParse (LocalReg, ForeignHint))
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (CmmParse LocalReg) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (CmmParse LocalReg)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (CmmParse CmmReg) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (CmmParse CmmReg)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Maybe [CmmParse LocalReg]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Maybe [CmmParse LocalReg])
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([CmmParse LocalReg]) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ([CmmParse LocalReg])
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([CmmParse LocalReg]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([CmmParse LocalReg])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (CmmParse LocalReg) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (CmmParse LocalReg)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (CmmType) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (CmmType)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (CmmType) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (CmmType)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyInTok :: (Located CmmToken) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Located CmmToken)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x03\x01\x00\x00\xf1\x03\x03\x01\x00\x00\x00\x00\x00\x04\x00\x00\xf4\x03\x00\x00\x32\x04\x31\x04\x30\x04\x2f\x04\x2e\x04\x2c\x04\xec\x03\xea\x03\x18\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x04\x18\x04\x00\x00\xe6\x03\x2a\x04\x29\x04\x14\x04\xe3\x03\xe2\x03\xe1\x03\xe0\x03\xdf\x03\xde\x03\x21\x04\xfc\x03\x00\x00\x00\x00\x98\x01\xdb\x03\x00\x00\x0c\x04\x0b\x04\x0a\x04\xfd\x03\xf0\x03\xee\x03\xbf\x03\x00\x00\xbc\x03\x00\x00\x16\x01\x00\x00\x00\x00\x0f\x01\xe9\x03\x00\x00\xb9\x03\xb8\x03\xa0\x03\x93\x03\x87\x03\xa3\x03\xbe\x03\x00\x00\xad\x03\xa1\x03\x00\x00\x00\x00\x20\x00\xb6\x03\x20\x00\x20\x00\xec\xff\xc9\x03\x23\x00\x00\x00\xce\x03\x98\x03\x60\x00\x4a\x00\x4a\x00\x4a\x00\xb0\x03\x85\x03\xa7\x03\x00\x00\x16\x00\x00\x00\xdb\x03\x00\x00\x95\x03\x8c\x03\x10\x00\x89\x03\x80\x03\x7d\x03\x00\x00\x83\x03\x0f\x01\xff\xff\x79\x03\x74\x03\x6d\x03\x02\x00\x20\x03\x27\x03\xab\x01\x59\x03\x00\x00\x49\x03\x00\x00\x4a\x00\x4a\x00\x14\x03\x4a\x00\x00\x00\x00\x00\x00\x00\x32\x03\x32\x03\x00\x00\x00\x00\x0f\x03\xfc\x02\xf7\x02\x00\x00\xdb\x03\xea\x02\x2b\x03\x4a\x00\x00\x00\x00\x00\x4a\x00\x4a\x00\x4a\x00\xe1\x02\x4a\x00\x6f\x02\xda\x01\x40\x02\x0c\x00\x00\x00\xbf\x02\x60\x00\x60\x00\x24\x03\x16\x03\x07\x03\x00\x00\x12\x03\x00\x00\xdb\x02\x4a\x00\x4a\x00\xd2\x02\xf9\x02\x00\x00\x00\x00\x00\x00\xd6\x02\xd5\x02\x97\x01\x0e\x02\x00\x00\x09\x03\xf4\x00\x06\x03\x00\x00\x00\x00\x14\x01\xfe\x02\x58\x02\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x20\x00\x60\x00\x60\x00\xc4\x02\x4a\x00\x03\x03\x06\x00\x4a\x00\xc8\x00\x97\x02\xfb\x02\x00\x00\xec\x02\xbf\x01\xfa\x02\xf3\x02\x83\x02\xeb\x02\xe2\x02\xe8\x02\xdd\x02\xd9\x02\xd8\x02\x00\x00\xdb\x03\x00\x00\x25\x00\xe5\x02\x00\x00\x58\x02\x4a\x00\xa5\x02\x00\x00\xde\x02\xcb\x02\x91\x02\xd1\x02\xd4\x02\xd3\x02\xbc\x02\xc1\x02\xb8\x02\xf4\x01\x00\x00\x4a\x00\x00\x00\x7a\x02\x68\x02\x67\x02\x00\x00\x66\x02\x00\x00\x00\x00\x9a\x02\xf7\x00\x50\x02\x53\x02\x85\x02\x4a\x00\xf7\x00\x00\x00\x92\x02\x80\x02\x00\x00\x7f\x02\x64\x02\x00\x00\x69\x02\x00\x00\xbe\x00\x59\x02\x6c\x02\xab\x02\xab\x02\xab\x02\xab\x02\xef\x01\xef\x01\xab\x02\xab\x02\xe4\x03\x71\x01\xed\x03\x25\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x02\x65\x02\x00\x00\x5c\x02\x00\x00\x5b\x02\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x39\x02\x56\x02\x14\x02\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x51\x02\x04\x02\x1e\x02\x03\x02\x00\x00\x00\x00\x34\x02\x33\x02\x32\x02\x2a\x02\x29\x02\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x01\xf6\x01\xeb\x01\xe9\x01\x26\x02\x19\x02\x00\x00\x22\x02\x24\x02\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x21\x02\x27\x02\x83\x01\xad\x00\x11\x02\x00\x00\x00\x02\x10\x02\x13\x02\x4a\x00\xd0\x01\x00\x00\x00\x00\x4a\x00\x20\x00\x05\x02\x07\x02\x00\x00\xc4\x01\x08\x00\xf5\x01\xdd\x01\xce\x01\xea\x01\x00\x00\xaa\x01\xa9\x01\x9e\x01\x00\x00\x20\x00\x9d\x01\x00\x00\x20\x00\xe0\x01\xdc\x01\xcf\x01\xa7\x01\x00\x00\x00\x00\x00\x00\xd4\x01\xa0\x01\xc2\x01\x00\x00\x00\x00\xbd\x01\xc1\x01\xac\x01\xa6\x01\x99\x01\x86\x01\x3d\x01\x32\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x01\x65\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xd1\x00\x00\x00\x00\x00\xcd\x00\x00\x00\x00\x00\x63\x01\x00\x00\x5a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x01\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x00\x00\x2f\x01\x58\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x01\x00\x00\x2d\x01\x00\x00\xb8\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\xe3\x00\xe0\x00\x00\x00\x00\x00\x1c\x01\x00\x00\xef\x00\x00\x00\x5c\x01\xc6\x03\xbd\x03\xba\x03\x00\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x0a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xb1\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x03\xa5\x03\x00\x00\xa2\x03\x00\x00\x00\x00\x00\x00\xe4\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x99\x03\x00\x00\x00\x00\x08\x03\x05\x03\x96\x03\x00\x00\xf1\x02\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x01\x45\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x8d\x03\x8a\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x03\x7e\x03\x75\x03\x72\x03\x69\x03\x66\x03\x5d\x03\x5a\x03\x51\x03\x4e\x03\x45\x03\x42\x03\x39\x03\x36\x03\x2d\x03\x2a\x03\xd8\x00\x43\x01\x41\x01\x00\x00\xee\x02\x00\x00\xc5\x00\xda\x02\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x03\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x00\x59\x00\x00\x00\x00\x00\x15\x03\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\x02\xf2\xff\x1e\x03\x11\x03\x1b\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x1c\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x00\x14\x00\x00\x00\x00\x00\xfd\x00\xd5\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfe\xff\x00\x00\x00\x00\xfe\xff\xfb\xff\xfc\xff\xeb\xff\xfa\xff\x00\x00\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\xff\x67\xff\x66\xff\x65\xff\x64\xff\x63\xff\x62\xff\x61\xff\x60\xff\x5f\xff\xe7\xff\x00\x00\xda\xff\xd8\xff\x00\x00\x00\x00\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\xff\xea\xff\xfd\xff\x00\x00\x6f\xff\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\xd7\xff\x00\x00\xdc\xff\xd9\xff\xf6\xff\x00\x00\xd5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\xff\x6c\xff\x00\x00\xec\xff\xe9\xff\xe0\xff\x00\x00\xe0\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\xff\x00\x00\x72\xff\x73\xff\x6a\xff\x6d\xff\x70\xff\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\x00\x00\xf6\xff\x00\x00\x68\xff\x00\x00\x69\xff\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x8d\xff\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x7c\xff\x7d\xff\x8e\xff\x89\xff\x89\xff\xf5\xff\xf8\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x6f\xff\x00\x00\x00\x00\x00\x00\x6b\xff\xd3\xff\x81\xff\x81\xff\x00\x00\x00\x00\x81\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x79\xff\x76\xff\x00\x00\x74\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xdf\xff\xe8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x75\xff\x00\x00\x78\xff\x00\x00\xcc\xff\xba\xff\x00\x00\xbe\xff\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\x00\x00\x00\x00\x00\x00\x81\xff\x00\x00\x00\x00\x81\xff\x00\x00\x7f\xff\x00\x00\x80\xff\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\xff\x00\x00\x90\xff\x93\xff\x00\x00\x94\xff\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\xff\x81\xff\x88\xff\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\xf9\xff\xed\xff\x00\x00\x00\x00\xaf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x73\xff\x00\x00\x00\x00\xb6\xff\x00\x00\xb3\xff\xca\xff\x00\x00\xc5\xff\xbb\xff\xbc\xff\x00\x00\x96\xff\x95\xff\x98\xff\x9a\xff\x9e\xff\x9f\xff\x97\xff\x99\xff\x9b\xff\x9c\xff\x9d\xff\xa0\xff\xa1\xff\xa2\xff\xa3\xff\xa4\xff\xb9\xff\x7a\xff\x77\xff\x00\x00\x00\x00\xd2\xff\x00\x00\xc1\xff\x00\x00\x81\xff\x87\xff\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\xb5\xff\xb4\xff\x00\x00\xc2\xff\x7e\xff\xcb\xff\x00\x00\xa7\xff\xaf\xff\x00\x00\xc3\xff\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\xff\x00\x00\xf0\xff\xef\xff\xf2\xff\xf1\xff\x92\xff\x8b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\xff\xae\xff\x00\x00\x00\x00\xb1\xff\xc9\xff\x81\xff\xb2\xff\xc7\xff\xc4\xff\x00\x00\x00\x00\x00\x00\x83\xff\x00\x00\x86\xff\x85\xff\x00\x00\x00\x00\x00\x00\xb8\xff\x82\xff\xd1\xff\x81\xff\xe0\xff\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\xad\xff\xe0\xff\x00\x00\xa9\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xc0\xff\xb7\xff\x84\xff\xcf\xff\x00\x00\x00\x00\x00\x00\xa5\xff\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xac\xff\xab\xff\xa8\xff\xc6\xff\xbf\xff\xd0\xff\x00\x00\x00\x00\xe4\xff\xe5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x04\x00\x05\x00\x0b\x00\x0c\x00\x07\x00\x12\x00\x06\x00\x10\x00\x0b\x00\x03\x00\x06\x00\x0e\x00\x0f\x00\x03\x00\x1c\x00\x1f\x00\x20\x00\x0d\x00\x22\x00\x23\x00\x24\x00\x01\x00\x08\x00\x27\x00\x02\x00\x17\x00\x30\x00\x07\x00\x32\x00\x07\x00\x1b\x00\x28\x00\x02\x00\x31\x00\x32\x00\x2c\x00\x16\x00\x07\x00\x05\x00\x15\x00\x31\x00\x32\x00\x20\x00\x21\x00\x1c\x00\x31\x00\x32\x00\x0c\x00\x0d\x00\x0e\x00\x2c\x00\x19\x00\x1a\x00\x1d\x00\x2c\x00\x1e\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x29\x00\x48\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x07\x00\x32\x00\x33\x00\x34\x00\x0b\x00\x36\x00\x37\x00\x0e\x00\x0f\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x07\x00\x09\x00\x46\x00\x09\x00\x0b\x00\x04\x00\x05\x00\x0e\x00\x0f\x00\x1f\x00\x20\x00\x19\x00\x1a\x00\x23\x00\x24\x00\x07\x00\x17\x00\x27\x00\x29\x00\x2a\x00\x2b\x00\x06\x00\x0e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x31\x00\x32\x00\x06\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x31\x00\x32\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x31\x00\x32\x00\x0d\x00\x0e\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x07\x00\x0b\x00\x0c\x00\x0a\x00\x07\x00\x0c\x00\x10\x00\x0a\x00\x17\x00\x0c\x00\x20\x00\x21\x00\x0b\x00\x0c\x00\x2b\x00\x0b\x00\x0c\x00\x10\x00\x20\x00\x11\x00\x10\x00\x0d\x00\x0e\x00\x0b\x00\x0c\x00\x27\x00\x0b\x00\x0c\x00\x10\x00\x16\x00\x28\x00\x10\x00\x47\x00\x21\x00\x2c\x00\x31\x00\x32\x00\x02\x00\x03\x00\x31\x00\x32\x00\x28\x00\x31\x00\x32\x00\x28\x00\x2c\x00\x31\x00\x32\x00\x2c\x00\x21\x00\x31\x00\x32\x00\x28\x00\x31\x00\x32\x00\x28\x00\x2c\x00\x45\x00\x46\x00\x2c\x00\x06\x00\x31\x00\x32\x00\x18\x00\x31\x00\x32\x00\x0b\x00\x0c\x00\x29\x00\x2a\x00\x2b\x00\x10\x00\x1f\x00\x20\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x25\x00\x26\x00\x27\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x0f\x00\x29\x00\x2a\x00\x31\x00\x32\x00\x03\x00\x22\x00\x0f\x00\x28\x00\x20\x00\x21\x00\x2d\x00\x2c\x00\x09\x00\x36\x00\x46\x00\x47\x00\x31\x00\x32\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x0f\x00\x46\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x13\x00\x14\x00\x13\x00\x14\x00\x13\x00\x14\x00\x13\x00\x14\x00\x46\x00\x47\x00\x46\x00\x47\x00\x1f\x00\x20\x00\x1f\x00\x20\x00\x1f\x00\x20\x00\x1f\x00\x20\x00\x27\x00\x0f\x00\x27\x00\x08\x00\x27\x00\x08\x00\x27\x00\x13\x00\x14\x00\x08\x00\x31\x00\x32\x00\x31\x00\x32\x00\x31\x00\x32\x00\x31\x00\x32\x00\x48\x00\x1f\x00\x20\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x27\x00\x47\x00\x02\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x1a\x00\x1b\x00\x31\x00\x32\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x02\x00\x03\x00\x16\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x08\x00\x16\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x04\x00\x03\x00\x02\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x35\x00\x02\x00\x08\x00\x31\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x05\x00\x04\x00\x07\x00\x02\x00\x46\x00\x16\x00\x47\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x47\x00\x47\x00\x08\x00\x16\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x06\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x03\x00\x16\x00\x48\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x06\x00\x02\x00\x16\x00\x47\x00\x08\x00\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x03\x00\x01\x00\x04\x00\x01\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x07\x00\x16\x00\x47\x00\x08\x00\x47\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x39\x00\x48\x00\x47\x00\x16\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x07\x00\x16\x00\x16\x00\x16\x00\x48\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x38\x00\x06\x00\x02\x00\x45\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x08\x00\x28\x00\x07\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x2e\x00\x04\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x09\x00\x20\x00\x16\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x06\x00\x06\x00\x02\x00\x38\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x08\x00\x48\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x48\x00\x48\x00\x48\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x47\x00\x0a\x00\x02\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x02\x00\x06\x00\x47\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x06\x00\x25\x00\x26\x00\x27\x00\x25\x00\x26\x00\x27\x00\x46\x00\x07\x00\x02\x00\x16\x00\x16\x00\x08\x00\x31\x00\x32\x00\x16\x00\x31\x00\x32\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x08\x00\x25\x00\x26\x00\x27\x00\x25\x00\x26\x00\x27\x00\x08\x00\x08\x00\x18\x00\x02\x00\x08\x00\x0e\x00\x31\x00\x32\x00\x46\x00\x31\x00\x32\x00\x1f\x00\x20\x00\x09\x00\x1f\x00\x20\x00\x09\x00\x25\x00\x26\x00\x27\x00\x25\x00\x26\x00\x27\x00\x47\x00\x08\x00\x46\x00\x46\x00\x16\x00\x08\x00\x31\x00\x32\x00\x46\x00\x31\x00\x32\x00\x1f\x00\x20\x00\x02\x00\x1f\x00\x20\x00\x48\x00\x25\x00\x26\x00\x27\x00\x25\x00\x26\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x08\x00\x1f\x00\x20\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x26\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x1f\x00\x20\x00\x31\x00\x32\x00\x48\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x19\x00\x1f\x00\x20\x00\x05\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x1f\x00\x20\x00\x46\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x0a\x00\x1f\x00\x20\x00\x46\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x1f\x00\x20\x00\x05\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x07\x00\x1f\x00\x20\x00\x05\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x04\x00\x1f\x00\x20\x00\x05\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x16\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x16\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x07\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x07\x00\x1f\x00\x20\x00\x04\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x16\x00\x1f\x00\x20\x00\x08\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x1f\x00\x20\x00\x09\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x48\x00\x1f\x00\x20\x00\x46\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x27\x00\x1f\x00\x20\x00\x46\x00\x48\x00\x46\x00\x01\x00\x31\x00\x32\x00\x27\x00\x31\x00\x32\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x31\x00\x32\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x1a\x00\x1b\x00\x48\x00\x48\x00\x46\x00\x07\x00\x16\x00\x46\x00\x16\x00\x1a\x00\x1b\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x16\x00\x46\x00\x47\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x16\x00\x16\x00\x16\x00\x02\x00\x46\x00\x46\x00\x46\x00\x46\x00\x46\x00\x46\x00\x16\x00\x02\x00\x46\x00\x03\x00\x16\x00\x02\x00\xff\xff\x47\x00\x46\x00\x07\x00\x34\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x46\x00\x4a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x79\x00\x82\x00\x6c\x00\x83\x01\x50\x00\x7a\x00\x7e\x01\xea\x00\x51\x00\x7b\x00\x74\x01\x06\x01\x7c\x00\x7d\x00\xc7\x00\x75\x01\x59\x01\x75\x00\x07\x01\x5a\x01\x5b\x01\x5c\x01\x8e\x00\x88\x00\x76\x00\x52\x01\x53\x01\xa5\x00\x8f\x00\xa6\x00\x53\x01\x72\x01\x52\x00\x56\x00\x77\x00\x09\x00\x53\x00\x89\x00\x57\x00\xa3\x00\x7a\x01\x54\x00\x09\x00\xc8\x00\xc9\x00\x4b\x01\x6d\x00\x6e\x00\xb7\x00\xb8\x00\xb9\x00\x75\x01\x4d\x01\x36\x01\x4e\x01\xca\x00\x55\x01\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x11\x00\xeb\x00\x08\x01\x58\x00\x59\x00\x5a\x00\x5b\x00\x7b\xff\x7a\x00\x7b\xff\x5c\x00\x5d\x00\x7b\x00\x13\x00\x5e\x00\x7c\x00\x7d\x00\x5f\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x60\x00\x61\x00\x9a\x00\x31\x01\x25\x00\x38\x01\x7b\x00\x6b\x00\x6c\x00\x7c\x00\x7d\x00\x59\x01\x75\x00\x35\x01\x36\x01\x7c\x01\x5c\x01\x7a\x00\x9b\x00\x76\x00\x20\x01\x9d\x00\x9e\x00\xed\x00\x7c\x00\x8c\x00\x4a\x00\x4b\x00\x09\x00\x77\x00\x09\x00\xfa\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x82\x00\xd9\x00\x49\x00\x4a\x00\x4b\x00\x09\x00\x6d\x00\x6e\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x82\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x7e\x00\x7f\x00\x80\x00\x81\x00\x82\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\xf3\x00\x09\x00\x3e\x00\x1f\x00\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\x2e\x00\x03\x00\x04\x00\x01\x01\x02\x00\x03\x00\x04\x00\x05\x00\x85\x01\x50\x00\x06\x00\x05\x00\x07\x00\x51\x00\x06\x00\x04\x01\x07\x00\xc8\x00\xc9\x00\x78\x01\x50\x00\xad\x00\x0d\x01\x50\x00\x51\x00\x45\x01\x24\x01\x51\x00\x1e\x00\x1f\x00\xa6\x00\x50\x00\x76\x00\xa7\x00\x50\x00\x51\x00\xcb\x00\x52\x00\x51\x00\x62\x01\xdd\x00\x53\x00\x77\x00\x09\x00\x4e\x00\x4f\x00\x54\x00\x09\x00\x52\x00\x08\x00\x09\x00\x52\x00\x53\x00\x08\x00\x09\x00\x53\x00\xdf\x00\x54\x00\x09\x00\x52\x00\x54\x00\x09\x00\x52\x00\x53\x00\x60\x00\x03\x01\x53\x00\x8a\x00\x54\x00\x09\x00\x90\x00\x54\x00\x09\x00\x4f\x00\x50\x00\x9c\x00\x9d\x00\x9e\x00\x51\x00\xcf\x00\x75\x00\x9f\x00\x4a\x00\x4b\x00\x09\x00\x79\x01\xd1\x00\x76\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x2b\x00\x11\x00\x12\x00\x77\x00\x09\x00\x3f\x00\x70\x00\x41\x00\x52\x00\xc8\x00\xc9\x00\x2f\x00\x53\x00\x4c\x00\x13\x00\xa1\x00\xa2\x00\x54\x00\x09\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x23\x00\x1e\x00\x71\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0b\x01\x97\x00\x0c\x01\x97\x00\xb2\x00\x97\x00\xb3\x00\xb4\x00\x21\x00\x22\x00\x21\x00\x22\x00\x98\x00\x75\x00\x98\x00\x75\x00\x98\x00\x75\x00\xb5\x00\x75\x00\x76\x00\x2b\x00\x76\x00\x2c\x00\x76\x00\x95\x01\x76\x00\x96\x00\x97\x00\x96\x01\x77\x00\x09\x00\x77\x00\x09\x00\x77\x00\x09\x00\x77\x00\x09\x00\x93\x01\x98\x00\x75\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\x76\x00\x94\x01\x63\x01\x48\x00\x49\x00\x4a\x00\x4b\x00\x09\x00\xc1\x00\xc2\x00\x77\x00\x09\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x24\x01\x4e\x00\x4f\x00\x8a\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\xe7\x00\x8c\x01\x8b\x01\x8d\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x8f\x01\xfe\x00\x8e\x01\x90\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x91\x01\x92\x01\x81\x01\x80\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xcd\x00\x82\x01\xce\x00\x83\x01\x85\x01\x70\x01\x87\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x88\x01\x89\x01\x6f\x01\x71\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\x40\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x77\x01\x72\x01\x4d\x01\x78\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\x23\x01\x7e\x01\x60\x01\x7c\x01\x5f\x01\x61\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x65\x01\x67\x01\x68\x01\x6a\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\x64\x01\x69\x01\x6b\x01\x47\x01\x6c\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x50\x01\x6d\x01\x6e\x01\x48\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xcb\x00\x49\x01\x4a\x01\x4b\x01\x4d\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x38\x01\x51\x01\x55\x01\x08\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xf2\x00\x57\x01\x28\x01\x29\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x2a\x01\x2b\x01\x2c\x01\x2d\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xcf\x00\xc8\x00\x2e\x01\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x2f\x01\x30\x01\x34\x01\x38\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x8c\x00\x31\x01\x35\x01\x3a\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x01\x01\x3b\x01\x3c\x01\x3d\x01\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\x00\x00\x00\x00\x3e\x01\x41\x01\x42\x01\x43\x01\xc1\x00\xc2\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xc0\x00\x44\x01\x45\x01\xec\x00\xed\x00\xc1\x00\xc2\x00\xc3\x00\xc4\x00\xc5\x00\xc6\x00\xcf\x00\x75\x00\x8c\x00\xcf\x00\x75\x00\xef\x00\x65\x01\xd1\x00\x76\x00\x5d\x01\xd1\x00\x76\x00\xf0\x00\xf3\x00\xfa\x00\xf5\x00\xf6\x00\xf8\x00\x77\x00\x09\x00\xf7\x00\x77\x00\x09\x00\xcf\x00\x75\x00\xf9\x00\xcf\x00\x75\x00\xfc\x00\x3e\x01\xd1\x00\x76\x00\x03\x01\xd1\x00\x76\x00\xfd\x00\x00\x01\xff\x00\x09\x01\x1f\x01\xaa\x00\x77\x00\x09\x00\x0b\x01\x77\x00\x09\x00\xcf\x00\x75\x00\x20\x01\xcf\x00\x75\x00\x22\x01\x09\x01\xd1\x00\x76\x00\xd0\x00\xd1\x00\x76\x00\xab\x00\xaf\x00\x26\x01\x27\x01\xb0\x00\xb1\x00\x77\x00\x09\x00\xa1\x00\x77\x00\x09\x00\xcf\x00\x75\x00\xb2\x00\xcf\x00\x75\x00\xd3\x00\xd4\x00\xd1\x00\x76\x00\xd5\x00\xd1\x00\x76\x00\x57\x01\x75\x00\xd9\x00\xd8\x00\xcf\x00\x75\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x32\x01\x76\x00\x58\x01\x75\x00\xdb\x00\xf0\x00\x75\x00\x77\x00\x09\x00\xdc\x00\x76\x00\x77\x00\x09\x00\x76\x00\x0e\x01\x75\x00\xdf\x00\x0f\x01\x75\x00\xe5\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x10\x01\x75\x00\xdd\x00\x11\x01\x75\x00\xe2\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x12\x01\x75\x00\xe6\x00\x13\x01\x75\x00\xe9\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x14\x01\x75\x00\xe8\x00\x15\x01\x75\x00\x72\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x16\x01\x75\x00\x73\x00\x17\x01\x75\x00\x74\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x18\x01\x75\x00\x84\x00\x19\x01\x75\x00\x92\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x1a\x01\x75\x00\x85\x00\x1b\x01\x75\x00\x86\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x1c\x01\x75\x00\x87\x00\x1d\x01\x75\x00\x8a\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\xab\x00\x75\x00\x8c\x00\xac\x00\x75\x00\x90\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\xd3\x00\x75\x00\x93\x00\xd6\x00\x75\x00\xa9\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\xe0\x00\x75\x00\x63\x00\xe2\x00\x75\x00\x64\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\xe3\x00\x75\x00\x66\x00\x74\x00\x75\x00\xa4\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x93\x00\x75\x00\x67\x00\x94\x00\x75\x00\x9c\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\x76\x00\x95\x00\x75\x00\x62\x00\x68\x00\x65\x00\x6b\x00\x77\x00\x09\x00\x76\x00\x77\x00\x09\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\x77\x00\x09\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xc1\x00\xc2\x00\x69\x00\x6a\x00\x41\x00\x31\x00\x43\x00\x25\x00\x44\x00\xc1\x00\xc2\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x45\x00\xa1\x00\xa2\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x46\x00\x47\x00\x48\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3c\x00\x3b\x00\x3d\x00\x3e\x00\x00\x00\x23\x00\x25\x00\x26\x00\x2e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x25\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 160) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160)
	]

happy_n_terms = 75 :: Int
happy_n_nonterms = 51 :: Int

happyReduce_1 = happySpecReduce_0  0# happyReduction_1
happyReduction_1  =  happyIn4
		 (return ()
	)

happyReduce_2 = happySpecReduce_2  0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_6 = happyMonadReduce 8# 1# happyReduction_6
happyReduction_6 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Name        happy_var_5)) -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	( withThisPackage $ \pkg -> 
                   do lits <- sequence happy_var_6;
                      staticClosure pkg happy_var_3 happy_var_5 (map getLit lits))}}}
	) (\r -> happyReturn (happyIn5 r))

happyReduce_7 = happyReduce 6# 2# happyReduction_7
happyReduction_7 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L _ (CmmT_String      happy_var_2)) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut8 happy_x_5 of { happy_var_5 -> 
	happyIn6
		 (do lbl <- happy_var_4;
                     ss <- sequence happy_var_5;
                     code (emitDecl (CmmData (section happy_var_2) (Statics lbl $ concat ss)))
	) `HappyStk` happyRest}}}

happyReduce_8 = happyMonadReduce 2# 3# happyReduction_8
happyReduction_8 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	( withThisPackage $ \pkg -> 
                   return (mkCmmDataLabel pkg happy_var_1))}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_9 = happySpecReduce_0  4# happyReduction_9
happyReduction_9  =  happyIn8
		 ([]
	)

happyReduce_10 = happySpecReduce_2  4# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_11 = happySpecReduce_3  5# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (do e <- happy_var_2;
                             return [CmmStaticLit (getLit e)]
	)}

happyReduce_12 = happySpecReduce_2  5# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (return [CmmUninitialised
                                                        (widthInBytes (typeWidth happy_var_1))]
	)}

happyReduce_13 = happyReduce 5# 5# happyReduction_13
happyReduction_13 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_4 of { (L _ (CmmT_String      happy_var_4)) -> 
	happyIn9
		 (return [mkString happy_var_4]
	) `HappyStk` happyRest}

happyReduce_14 = happyReduce 5# 5# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Int         happy_var_3)) -> 
	happyIn9
		 (return [CmmUninitialised 
                                                        (fromIntegral happy_var_3)]
	) `HappyStk` happyRest}

happyReduce_15 = happyReduce 5# 5# happyReduction_15
happyReduction_15 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_Int         happy_var_3)) -> 
	happyIn9
		 (return [CmmUninitialised 
                                                (widthInBytes (typeWidth happy_var_1) * 
                                                        fromIntegral happy_var_3)]
	) `HappyStk` happyRest}}

happyReduce_16 = happyReduce 5# 5# happyReduction_16
happyReduction_16 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	happyIn9
		 (do { lits <- sequence happy_var_4
                ; dflags <- getDynFlags
                     ; return $ map CmmStaticLit $
                        mkStaticClosure dflags (mkForeignLabel happy_var_3 Nothing ForeignLabelInExternalPackage IsData)
                         -- mkForeignLabel because these are only used
                         -- for CHARLIKE and INTLIKE closures in the RTS.
                        dontCareCCS (map getLit lits) [] [] [] }
	) `HappyStk` happyRest}}

happyReduce_17 = happySpecReduce_0  6# happyReduction_17
happyReduction_17  =  happyIn10
		 ([]
	)

happyReduce_18 = happySpecReduce_3  6# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (happy_var_2 : happy_var_3
	)}}

happyReduce_19 = happyReduce 4# 7# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn11
		 (do ((entry_ret_label, info, stk_formals, formals), agraph) <-
                       getCodeR $ loopDecls $ do {
                         (entry_ret_label, info, stk_formals) <- happy_var_1;
                         formals <- sequence (fromMaybe [] happy_var_3);
                         happy_var_4;
                         return (entry_ret_label, info, stk_formals, formals) }
                     let do_layout = isJust happy_var_3
                     code (emitProcWithStackFrame happy_var_2 info
                                entry_ret_label stk_formals formals agraph
                                do_layout )
	) `HappyStk` happyRest}}}}

happyReduce_20 = happySpecReduce_0  8# happyReduction_20
happyReduction_20  =  happyIn12
		 (NativeNodeCall
	)

happyReduce_21 = happySpecReduce_1  8# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn12
		 (NativeReturn
	)

happyReduce_22 = happySpecReduce_1  9# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn13
		 (return ()
	)

happyReduce_23 = happySpecReduce_3  9# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (happy_var_2
	)}

happyReduce_24 = happyMonadReduce 1# 10# happyReduction_24
happyReduction_24 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	( withThisPackage $ \pkg ->
                   do   newFunctionName happy_var_1 pkg
                        return (mkCmmCodeLabel pkg happy_var_1, Nothing, []))}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_25 = happyMonadReduce 14# 10# happyReduction_25
happyReduction_25 (happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int         happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int         happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String      happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String      happy_var_13)) -> 
	( withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags happy_var_11 happy_var_13
                          rep  = mkRTSRep (fromIntegral happy_var_9) $
                                   mkHeapRep dflags False (fromIntegral happy_var_5)
                                                   (fromIntegral happy_var_7) Thunk
                              -- not really Thunk, but that makes the info table
                              -- we want.
                      return (mkCmmEntryLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              []))}}}}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_26 = happyMonadReduce 16# 10# happyReduction_26
happyReduction_26 (happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int         happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int         happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String      happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String      happy_var_13)) -> 
	case happyOutTok happy_x_15 of { (L _ (CmmT_Int         happy_var_15)) -> 
	( withThisPackage $ \pkg -> 
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags happy_var_11 happy_var_13
                          ty   = Fun 0 (ArgSpec (fromIntegral happy_var_15))
                                -- Arity zero, arg_type happy_var_15
                          rep = mkRTSRep (fromIntegral happy_var_9) $
                                    mkHeapRep dflags False (fromIntegral happy_var_5)
                                                    (fromIntegral happy_var_7) ty
                      return (mkCmmEntryLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              []))}}}}}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_27 = happyMonadReduce 16# 10# happyReduction_27
happyReduction_27 (happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int         happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int         happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_Int         happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String      happy_var_13)) -> 
	case happyOutTok happy_x_15 of { (L _ (CmmT_String      happy_var_15)) -> 
	( withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags happy_var_13 happy_var_15
                          ty  = Constr (fromIntegral happy_var_9)  -- Tag
                                       (stringToWord8s happy_var_13)
                          rep = mkRTSRep (fromIntegral happy_var_11) $
                                  mkHeapRep dflags False (fromIntegral happy_var_5)
                                                  (fromIntegral happy_var_7) ty
                      return (mkCmmEntryLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              []))}}}}}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_28 = happyMonadReduce 12# 10# happyReduction_28
happyReduction_28 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int         happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_String      happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String      happy_var_11)) -> 
	( withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      let prof = profilingInfo dflags happy_var_9 happy_var_11
                          ty  = ThunkSelector (fromIntegral happy_var_5)
                          rep = mkRTSRep (fromIntegral happy_var_7) $
                                   mkHeapRep dflags False 0 0 ty
                      return (mkCmmEntryLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              []))}}}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_29 = happyMonadReduce 6# 10# happyReduction_29
happyReduction_29 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	( withThisPackage $ \pkg ->
                   do let prof = NoProfilingInfo
                          rep  = mkRTSRep (fromIntegral happy_var_5) $ mkStackRep []
                      return (mkCmmRetLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              []))}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_30 = happyMonadReduce 8# 10# happyReduction_30
happyReduction_30 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int         happy_var_5)) -> 
	case happyOut50 happy_x_7 of { happy_var_7 -> 
	( withThisPackage $ \pkg ->
                   do dflags <- getDynFlags
                      live <- sequence happy_var_7
                      let prof = NoProfilingInfo
                          -- drop one for the info pointer
                          bitmap = mkLiveness dflags (map Just (drop 1 live))
                          rep  = mkRTSRep (fromIntegral happy_var_5) $ mkStackRep bitmap
                      return (mkCmmRetLabel pkg happy_var_3,
                              Just $ CmmInfoTable { cit_lbl = mkCmmRetInfoLabel pkg happy_var_3
                                           , cit_rep = rep
                                           , cit_prof = prof, cit_srt = NoC_SRT },
                              live))}}}
	) (\r -> happyReturn (happyIn14 r))

happyReduce_31 = happySpecReduce_0  11# happyReduction_31
happyReduction_31  =  happyIn15
		 (return ()
	)

happyReduce_32 = happySpecReduce_2  11# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_33 = happySpecReduce_2  11# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_34 = happySpecReduce_3  12# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (mapM_ (newLocal happy_var_1) happy_var_2
	)}}

happyReduce_35 = happySpecReduce_3  12# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (mapM_ newImport happy_var_2
	)}

happyReduce_36 = happySpecReduce_3  12# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn16
		 (return ()
	)

happyReduce_37 = happySpecReduce_1  13# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ([happy_var_1]
	)}

happyReduce_38 = happySpecReduce_3  13# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  14# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn18
		 ((happy_var_1, mkForeignLabel happy_var_1 Nothing ForeignLabelInExternalPackage IsFunction)
	)}

happyReduce_40 = happySpecReduce_2  14# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_String      happy_var_1)) -> 
	case happyOutTok happy_x_2 of { (L _ (CmmT_Name        happy_var_2)) -> 
	happyIn18
		 ((happy_var_2, mkCmmCodeLabel (fsToPackageId (mkFastString happy_var_1)) happy_var_2)
	)}}

happyReduce_41 = happySpecReduce_1  15# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn19
		 ([happy_var_1]
	)}

happyReduce_42 = happySpecReduce_3  15# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_43 = happySpecReduce_1  16# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn20
		 (return ()
	)

happyReduce_44 = happySpecReduce_2  16# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn20
		 (do l <- newLabel happy_var_1; emitLabel l
	)}

happyReduce_45 = happyReduce 4# 16# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (do reg <- happy_var_1; e <- happy_var_3; emitAssign reg e
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 7# 16# happyReduction_46
happyReduction_46 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	happyIn20
		 (doStore happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_47 = happyMonadReduce 10# 16# happyReduction_47
happyReduction_47 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_String      happy_var_3)) -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	case happyOut25 happy_x_8 of { happy_var_8 -> 
	case happyOut22 happy_x_9 of { happy_var_9 -> 
	( foreignCall happy_var_3 happy_var_1 happy_var_4 happy_var_6 happy_var_8 happy_var_9)}}}}}}
	) (\r -> happyReturn (happyIn20 r))

happyReduce_48 = happyMonadReduce 8# 16# happyReduction_48
happyReduction_48 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { (L _ (CmmT_Name        happy_var_4)) -> 
	case happyOut41 happy_x_6 of { happy_var_6 -> 
	( primCall happy_var_1 happy_var_4 happy_var_6)}}}
	) (\r -> happyReturn (happyIn20 r))

happyReduce_49 = happyMonadReduce 5# 16# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	( stmtMacro happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn20 r))

happyReduce_50 = happyReduce 7# 16# happyReduction_50
happyReduction_50 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	case happyOut33 happy_x_6 of { happy_var_6 -> 
	happyIn20
		 (do as <- sequence happy_var_5; doSwitch happy_var_2 happy_var_3 as happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_51 = happySpecReduce_3  16# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (L _ (CmmT_Name        happy_var_2)) -> 
	happyIn20
		 (do l <- lookupLabel happy_var_2; emit (mkBranch l)
	)}

happyReduce_52 = happyReduce 5# 16# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (doReturn happy_var_3
	) `HappyStk` happyRest}

happyReduce_53 = happyReduce 4# 16# happyReduction_53
happyReduction_53 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (doRawJump happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_54 = happyReduce 6# 16# happyReduction_54
happyReduction_54 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (doJumpWithStack happy_var_2 [] happy_var_4
	) `HappyStk` happyRest}}

happyReduce_55 = happyReduce 9# 16# happyReduction_55
happyReduction_55 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	case happyOut41 happy_x_7 of { happy_var_7 -> 
	happyIn20
		 (doJumpWithStack happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_56 = happyReduce 6# 16# happyReduction_56
happyReduction_56 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (doCall happy_var_2 [] happy_var_4
	) `HappyStk` happyRest}}

happyReduce_57 = happyReduce 10# 16# happyReduction_57
happyReduction_57 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut51 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	case happyOut41 happy_x_8 of { happy_var_8 -> 
	happyIn20
		 (doCall happy_var_6 happy_var_2 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_58 = happyReduce 4# 16# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { (L _ (CmmT_Name        happy_var_4)) -> 
	happyIn20
		 (do l <- lookupLabel happy_var_4; cmmRawIf happy_var_2 l
	) `HappyStk` happyRest}}

happyReduce_59 = happyReduce 6# 16# happyReduction_59
happyReduction_59 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut15 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_6 of { happy_var_6 -> 
	happyIn20
		 (cmmIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_60 = happyReduce 5# 16# happyReduction_60
happyReduction_60 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut41 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn20
		 (pushStackFrame happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 5# 16# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_5 of { happy_var_5 -> 
	happyIn20
		 (reserveStackFrame happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_62 = happySpecReduce_1  17# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn21
		 (return (CmmLit (CmmLabel (mkForeignLabel happy_var_1 Nothing ForeignLabelInThisPackage IsFunction)))
	)}

happyReduce_63 = happySpecReduce_0  18# happyReduction_63
happyReduction_63  =  happyIn22
		 (CmmMayReturn
	)

happyReduce_64 = happySpecReduce_2  18# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  happyIn22
		 (CmmNeverReturns
	)

happyReduce_65 = happySpecReduce_1  19# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  19# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (do e <- happy_var_1; return (BoolTest e)
	)}

happyReduce_67 = happySpecReduce_3  20# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (do e1 <- happy_var_1; e2 <- happy_var_3; 
                                          return (BoolAnd e1 e2)
	)}}

happyReduce_68 = happySpecReduce_3  20# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (do e1 <- happy_var_1; e2 <- happy_var_3; 
                                          return (BoolOr e1 e2)
	)}}

happyReduce_69 = happySpecReduce_2  20# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (do e <- happy_var_2; return (BoolNot e)
	)}

happyReduce_70 = happySpecReduce_3  20# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (happy_var_2
	)}

happyReduce_71 = happySpecReduce_0  21# happyReduction_71
happyReduction_71  =  happyIn25
		 (PlayRisky
	)

happyReduce_72 = happyMonadReduce 1# 21# happyReduction_72
happyReduction_72 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String      happy_var_1)) -> 
	( parseSafety happy_var_1)}
	) (\r -> happyReturn (happyIn25 r))

happyReduce_73 = happySpecReduce_2  22# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  happyIn26
		 ([]
	)

happyReduce_74 = happyMonadReduce 3# 22# happyReduction_74
happyReduction_74 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (( do df <- getDynFlags
                                         ; return (realArgRegsCover df))
	) (\r -> happyReturn (happyIn26 r))

happyReduce_75 = happySpecReduce_3  22# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 (happy_var_2
	)}

happyReduce_76 = happySpecReduce_1  23# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn27
		 ([happy_var_1]
	)}

happyReduce_77 = happySpecReduce_3  23# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_78 = happyReduce 5# 24# happyReduction_78
happyReduction_78 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L _ (CmmT_Int         happy_var_2)) -> 
	case happyOutTok happy_x_4 of { (L _ (CmmT_Int         happy_var_4)) -> 
	happyIn28
		 (Just (fromIntegral happy_var_2, fromIntegral happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_79 = happySpecReduce_0  24# happyReduction_79
happyReduction_79  =  happyIn28
		 (Nothing
	)

happyReduce_80 = happySpecReduce_0  25# happyReduction_80
happyReduction_80  =  happyIn29
		 ([]
	)

happyReduce_81 = happySpecReduce_2  25# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_82 = happyReduce 4# 26# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (do b <- happy_var_4; return (happy_var_2, b)
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_3  27# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (return (Right happy_var_2)
	)}

happyReduce_84 = happySpecReduce_3  27# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (L _ (CmmT_Name        happy_var_2)) -> 
	happyIn31
		 (do l <- lookupLabel happy_var_2; return (Left l)
	)}

happyReduce_85 = happySpecReduce_1  28# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int         happy_var_1)) -> 
	happyIn32
		 ([ fromIntegral happy_var_1 ]
	)}

happyReduce_86 = happySpecReduce_3  28# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int         happy_var_1)) -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (fromIntegral happy_var_1 : happy_var_3
	)}}

happyReduce_87 = happyReduce 5# 29# happyReduction_87
happyReduction_87 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (Just happy_var_4
	) `HappyStk` happyRest}

happyReduce_88 = happySpecReduce_0  29# happyReduction_88
happyReduction_88  =  happyIn33
		 (Nothing
	)

happyReduce_89 = happySpecReduce_0  30# happyReduction_89
happyReduction_89  =  happyIn34
		 (return ()
	)

happyReduce_90 = happyReduce 4# 30# happyReduction_90
happyReduction_90 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (happy_var_3
	) `HappyStk` happyRest}

happyReduce_91 = happySpecReduce_3  31# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Quot [happy_var_1,happy_var_3]
	)}}

happyReduce_92 = happySpecReduce_3  31# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Mul [happy_var_1,happy_var_3]
	)}}

happyReduce_93 = happySpecReduce_3  31# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Rem [happy_var_1,happy_var_3]
	)}}

happyReduce_94 = happySpecReduce_3  31# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Sub [happy_var_1,happy_var_3]
	)}}

happyReduce_95 = happySpecReduce_3  31# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Add [happy_var_1,happy_var_3]
	)}}

happyReduce_96 = happySpecReduce_3  31# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Shr [happy_var_1,happy_var_3]
	)}}

happyReduce_97 = happySpecReduce_3  31# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Shl [happy_var_1,happy_var_3]
	)}}

happyReduce_98 = happySpecReduce_3  31# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_And [happy_var_1,happy_var_3]
	)}}

happyReduce_99 = happySpecReduce_3  31# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Xor [happy_var_1,happy_var_3]
	)}}

happyReduce_100 = happySpecReduce_3  31# happyReduction_100
happyReduction_100 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Or [happy_var_1,happy_var_3]
	)}}

happyReduce_101 = happySpecReduce_3  31# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Ge [happy_var_1,happy_var_3]
	)}}

happyReduce_102 = happySpecReduce_3  31# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Gt [happy_var_1,happy_var_3]
	)}}

happyReduce_103 = happySpecReduce_3  31# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Le [happy_var_1,happy_var_3]
	)}}

happyReduce_104 = happySpecReduce_3  31# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_U_Lt [happy_var_1,happy_var_3]
	)}}

happyReduce_105 = happySpecReduce_3  31# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Ne [happy_var_1,happy_var_3]
	)}}

happyReduce_106 = happySpecReduce_3  31# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (mkMachOp MO_Eq [happy_var_1,happy_var_3]
	)}}

happyReduce_107 = happySpecReduce_2  31# happyReduction_107
happyReduction_107 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (mkMachOp MO_Not [happy_var_2]
	)}

happyReduce_108 = happySpecReduce_2  31# happyReduction_108
happyReduction_108 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (mkMachOp MO_S_Neg [happy_var_2]
	)}

happyReduce_109 = happyMonadReduce 5# 31# happyReduction_109
happyReduction_109 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_Name        happy_var_3)) -> 
	case happyOut36 happy_x_5 of { happy_var_5 -> 
	( do { mo <- nameToMachOp happy_var_3 ;
                                                return (mkMachOp mo [happy_var_1,happy_var_5]) })}}}
	) (\r -> happyReturn (happyIn35 r))

happyReduce_110 = happySpecReduce_1  31# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_111 = happySpecReduce_2  32# happyReduction_111
happyReduction_111 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int         happy_var_1)) -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (return (CmmLit (CmmInt happy_var_1 (typeWidth happy_var_2)))
	)}}

happyReduce_112 = happySpecReduce_2  32# happyReduction_112
happyReduction_112 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Float       happy_var_1)) -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (return (CmmLit (CmmFloat happy_var_1 (typeWidth happy_var_2)))
	)}}

happyReduce_113 = happySpecReduce_1  32# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_String      happy_var_1)) -> 
	happyIn36
		 (do s <- code (newStringCLit happy_var_1); 
                                      return (CmmLit s)
	)}

happyReduce_114 = happySpecReduce_1  32# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_115 = happyReduce 4# 32# happyReduction_115
happyReduction_115 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (do e <- happy_var_3; return (CmmLoad e happy_var_1)
	) `HappyStk` happyRest}}

happyReduce_116 = happyMonadReduce 5# 32# happyReduction_116
happyReduction_116 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_2 of { (L _ (CmmT_Name        happy_var_2)) -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	( exprOp happy_var_2 happy_var_4)}}
	) (\r -> happyReturn (happyIn36 r))

happyReduce_117 = happySpecReduce_3  32# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (happy_var_2
	)}

happyReduce_118 = happyMonadReduce 0# 33# happyReduction_118
happyReduction_118 (happyRest) tk
	 = happyThen (( do dflags <- getDynFlags; return $ bWord dflags)
	) (\r -> happyReturn (happyIn37 r))

happyReduce_119 = happySpecReduce_2  33# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (happy_var_2
	)}

happyReduce_120 = happySpecReduce_0  34# happyReduction_120
happyReduction_120  =  happyIn38
		 ([]
	)

happyReduce_121 = happySpecReduce_1  34# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_122 = happySpecReduce_1  35# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ([happy_var_1]
	)}

happyReduce_123 = happySpecReduce_3  35# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_124 = happySpecReduce_1  36# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (do e <- happy_var_1;
                                             return (e, inferCmmHint e)
	)}

happyReduce_125 = happyMonadReduce 2# 36# happyReduction_125
happyReduction_125 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L _ (CmmT_String      happy_var_2)) -> 
	( do h <- parseCmmHint happy_var_2;
                                              return $ do
                                                e <- happy_var_1; return (e, h))}}
	) (\r -> happyReturn (happyIn40 r))

happyReduce_126 = happySpecReduce_0  37# happyReduction_126
happyReduction_126  =  happyIn41
		 ([]
	)

happyReduce_127 = happySpecReduce_1  37# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_128 = happySpecReduce_1  38# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ([ happy_var_1 ]
	)}

happyReduce_129 = happySpecReduce_3  38# happyReduction_129
happyReduction_129 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_130 = happySpecReduce_1  39# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn43
		 (lookupName happy_var_1
	)}

happyReduce_131 = happySpecReduce_1  39# happyReduction_131
happyReduction_131 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn43
		 (return (CmmReg (CmmGlobal happy_var_1))
	)}

happyReduce_132 = happySpecReduce_0  40# happyReduction_132
happyReduction_132  =  happyIn44
		 ([]
	)

happyReduce_133 = happyReduce 4# 40# happyReduction_133
happyReduction_133 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (happy_var_2
	) `HappyStk` happyRest}

happyReduce_134 = happySpecReduce_1  41# happyReduction_134
happyReduction_134 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ([happy_var_1]
	)}

happyReduce_135 = happySpecReduce_2  41# happyReduction_135
happyReduction_135 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ([happy_var_1]
	)}

happyReduce_136 = happySpecReduce_3  41# happyReduction_136
happyReduction_136 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_137 = happySpecReduce_1  42# happyReduction_137
happyReduction_137 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (do e <- happy_var_1; return (e, (inferCmmHint (CmmReg (CmmLocal e))))
	)}

happyReduce_138 = happyMonadReduce 2# 42# happyReduction_138
happyReduction_138 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String      happy_var_1)) -> 
	case happyOut47 happy_x_2 of { happy_var_2 -> 
	( do h <- parseCmmHint happy_var_1;
                                      return $ do
                                         e <- happy_var_2; return (e,h))}}
	) (\r -> happyReturn (happyIn46 r))

happyReduce_139 = happySpecReduce_1  43# happyReduction_139
happyReduction_139 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn47
		 (do e <- lookupName happy_var_1;
                                     return $
                                       case e of 
                                        CmmReg (CmmLocal r) -> r
                                        other -> pprPanic "CmmParse:" (ftext happy_var_1 <> text " not a local register")
	)}

happyReduce_140 = happySpecReduce_1  44# happyReduction_140
happyReduction_140 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name        happy_var_1)) -> 
	happyIn48
		 (do e <- lookupName happy_var_1;
                                     return $
                                       case e of 
                                        CmmReg r -> r
                                        other -> pprPanic "CmmParse:" (ftext happy_var_1 <> text " not a register")
	)}

happyReduce_141 = happySpecReduce_1  44# happyReduction_141
happyReduction_141 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn48
		 (return (CmmGlobal happy_var_1)
	)}

happyReduce_142 = happySpecReduce_0  45# happyReduction_142
happyReduction_142  =  happyIn49
		 (Nothing
	)

happyReduce_143 = happySpecReduce_3  45# happyReduction_143
happyReduction_143 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (Just happy_var_2
	)}

happyReduce_144 = happySpecReduce_0  46# happyReduction_144
happyReduction_144  =  happyIn50
		 ([]
	)

happyReduce_145 = happySpecReduce_1  46# happyReduction_145
happyReduction_145 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_146 = happySpecReduce_2  47# happyReduction_146
happyReduction_146 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ([happy_var_1]
	)}

happyReduce_147 = happySpecReduce_1  47# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ([happy_var_1]
	)}

happyReduce_148 = happySpecReduce_3  47# happyReduction_148
happyReduction_148 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_149 = happySpecReduce_2  48# happyReduction_149
happyReduction_149 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L _ (CmmT_Name        happy_var_2)) -> 
	happyIn52
		 (newLocal happy_var_1 happy_var_2
	)}}

happyReduce_150 = happySpecReduce_1  49# happyReduction_150
happyReduction_150 happy_x_1
	 =  happyIn53
		 (b8
	)

happyReduce_151 = happySpecReduce_1  49# happyReduction_151
happyReduction_151 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_152 = happySpecReduce_1  50# happyReduction_152
happyReduction_152 happy_x_1
	 =  happyIn54
		 (b16
	)

happyReduce_153 = happySpecReduce_1  50# happyReduction_153
happyReduction_153 happy_x_1
	 =  happyIn54
		 (b32
	)

happyReduce_154 = happySpecReduce_1  50# happyReduction_154
happyReduction_154 happy_x_1
	 =  happyIn54
		 (b64
	)

happyReduce_155 = happySpecReduce_1  50# happyReduction_155
happyReduction_155 happy_x_1
	 =  happyIn54
		 (b128
	)

happyReduce_156 = happySpecReduce_1  50# happyReduction_156
happyReduction_156 happy_x_1
	 =  happyIn54
		 (b256
	)

happyReduce_157 = happySpecReduce_1  50# happyReduction_157
happyReduction_157 happy_x_1
	 =  happyIn54
		 (b512
	)

happyReduce_158 = happySpecReduce_1  50# happyReduction_158
happyReduction_158 happy_x_1
	 =  happyIn54
		 (f32
	)

happyReduce_159 = happySpecReduce_1  50# happyReduction_159
happyReduction_159 happy_x_1
	 =  happyIn54
		 (f64
	)

happyReduce_160 = happyMonadReduce 1# 50# happyReduction_160
happyReduction_160 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (( do dflags <- getDynFlags; return $ gcWord dflags)
	) (\r -> happyReturn (happyIn54 r))

happyNewToken action sts stk
	= cmmlex(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ CmmT_EOF -> happyDoAction 74# tk action sts stk;
	L _ (CmmT_SpecChar ':') -> cont 1#;
	L _ (CmmT_SpecChar ';') -> cont 2#;
	L _ (CmmT_SpecChar '{') -> cont 3#;
	L _ (CmmT_SpecChar '}') -> cont 4#;
	L _ (CmmT_SpecChar '[') -> cont 5#;
	L _ (CmmT_SpecChar ']') -> cont 6#;
	L _ (CmmT_SpecChar '(') -> cont 7#;
	L _ (CmmT_SpecChar ')') -> cont 8#;
	L _ (CmmT_SpecChar '=') -> cont 9#;
	L _ (CmmT_SpecChar '`') -> cont 10#;
	L _ (CmmT_SpecChar '~') -> cont 11#;
	L _ (CmmT_SpecChar '/') -> cont 12#;
	L _ (CmmT_SpecChar '*') -> cont 13#;
	L _ (CmmT_SpecChar '%') -> cont 14#;
	L _ (CmmT_SpecChar '-') -> cont 15#;
	L _ (CmmT_SpecChar '+') -> cont 16#;
	L _ (CmmT_SpecChar '&') -> cont 17#;
	L _ (CmmT_SpecChar '^') -> cont 18#;
	L _ (CmmT_SpecChar '|') -> cont 19#;
	L _ (CmmT_SpecChar '>') -> cont 20#;
	L _ (CmmT_SpecChar '<') -> cont 21#;
	L _ (CmmT_SpecChar ',') -> cont 22#;
	L _ (CmmT_SpecChar '!') -> cont 23#;
	L _ (CmmT_DotDot) -> cont 24#;
	L _ (CmmT_DoubleColon) -> cont 25#;
	L _ (CmmT_Shr) -> cont 26#;
	L _ (CmmT_Shl) -> cont 27#;
	L _ (CmmT_Ge) -> cont 28#;
	L _ (CmmT_Le) -> cont 29#;
	L _ (CmmT_Eq) -> cont 30#;
	L _ (CmmT_Ne) -> cont 31#;
	L _ (CmmT_BoolAnd) -> cont 32#;
	L _ (CmmT_BoolOr) -> cont 33#;
	L _ (CmmT_CLOSURE) -> cont 34#;
	L _ (CmmT_INFO_TABLE) -> cont 35#;
	L _ (CmmT_INFO_TABLE_RET) -> cont 36#;
	L _ (CmmT_INFO_TABLE_FUN) -> cont 37#;
	L _ (CmmT_INFO_TABLE_CONSTR) -> cont 38#;
	L _ (CmmT_INFO_TABLE_SELECTOR) -> cont 39#;
	L _ (CmmT_else) -> cont 40#;
	L _ (CmmT_export) -> cont 41#;
	L _ (CmmT_section) -> cont 42#;
	L _ (CmmT_align) -> cont 43#;
	L _ (CmmT_goto) -> cont 44#;
	L _ (CmmT_if) -> cont 45#;
	L _ (CmmT_call) -> cont 46#;
	L _ (CmmT_jump) -> cont 47#;
	L _ (CmmT_foreign) -> cont 48#;
	L _ (CmmT_never) -> cont 49#;
	L _ (CmmT_prim) -> cont 50#;
	L _ (CmmT_reserve) -> cont 51#;
	L _ (CmmT_return) -> cont 52#;
	L _ (CmmT_returns) -> cont 53#;
	L _ (CmmT_import) -> cont 54#;
	L _ (CmmT_switch) -> cont 55#;
	L _ (CmmT_case) -> cont 56#;
	L _ (CmmT_default) -> cont 57#;
	L _ (CmmT_push) -> cont 58#;
	L _ (CmmT_bits8) -> cont 59#;
	L _ (CmmT_bits16) -> cont 60#;
	L _ (CmmT_bits32) -> cont 61#;
	L _ (CmmT_bits64) -> cont 62#;
	L _ (CmmT_bits128) -> cont 63#;
	L _ (CmmT_bits256) -> cont 64#;
	L _ (CmmT_bits512) -> cont 65#;
	L _ (CmmT_float32) -> cont 66#;
	L _ (CmmT_float64) -> cont 67#;
	L _ (CmmT_gcptr) -> cont 68#;
	L _ (CmmT_GlobalReg   happy_dollar_dollar) -> cont 69#;
	L _ (CmmT_Name        happy_dollar_dollar) -> cont 70#;
	L _ (CmmT_String      happy_dollar_dollar) -> cont 71#;
	L _ (CmmT_Int         happy_dollar_dollar) -> cont 72#;
	L _ (CmmT_Float       happy_dollar_dollar) -> cont 73#;
	_ -> happyError' tk
	})

happyError_ 74# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Located CmmToken) -> P a
happyError' tk = (\token -> happyError) tk

cmmParse = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDoSeq


section :: String -> Section
section "text"      = Text
section "data"      = Data
section "rodata"    = ReadOnlyData
section "relrodata" = RelocatableReadOnlyData
section "bss"       = UninitialisedData
section s           = OtherSection s

mkString :: String -> CmmStatic
mkString s = CmmString (map (fromIntegral.ord) s)

-- |
-- Given an info table, decide what the entry convention for the proc
-- is.  That is, for an INFO_TABLE_RET we want the return convention,
-- otherwise it is a NativeNodeCall.
--
infoConv :: Maybe CmmInfoTable -> Convention
infoConv Nothing = NativeNodeCall
infoConv (Just info)
  | isStackRep (cit_rep info) = NativeReturn
  | otherwise                 = NativeNodeCall

-- mkMachOp infers the type of the MachOp from the type of its first
-- argument.  We assume that this is correct: for MachOps that don't have
-- symmetrical args (e.g. shift ops), the first arg determines the type of
-- the op.
mkMachOp :: (Width -> MachOp) -> [CmmParse CmmExpr] -> CmmParse CmmExpr
mkMachOp fn args = do
  dflags <- getDynFlags
  arg_exprs <- sequence args
  return (CmmMachOp (fn (typeWidth (cmmExprType dflags (head arg_exprs)))) arg_exprs)

getLit :: CmmExpr -> CmmLit
getLit (CmmLit l) = l
getLit (CmmMachOp (MO_S_Neg _) [CmmLit (CmmInt i r)])  = CmmInt (negate i) r
getLit _ = panic "invalid literal" -- TODO messy failure

nameToMachOp :: FastString -> P (Width -> MachOp)
nameToMachOp name =
  case lookupUFM machOps name of
        Nothing -> fail ("unknown primitive " ++ unpackFS name)
        Just m  -> return m

exprOp :: FastString -> [CmmParse CmmExpr] -> P (CmmParse CmmExpr)
exprOp name args_code = do
  dflags <- getDynFlags
  case lookupUFM (exprMacros dflags) name of
     Just f  -> return $ do
        args <- sequence args_code
        return (f args)
     Nothing -> do
        mo <- nameToMachOp name
        return $ mkMachOp mo args_code

exprMacros :: DynFlags -> UniqFM ([CmmExpr] -> CmmExpr)
exprMacros dflags = listToUFM [
  ( fsLit "ENTRY_CODE",   \ [x] -> entryCode dflags x ),
  ( fsLit "INFO_PTR",     \ [x] -> closureInfoPtr dflags x ),
  ( fsLit "STD_INFO",     \ [x] -> infoTable dflags x ),
  ( fsLit "FUN_INFO",     \ [x] -> funInfoTable dflags x ),
  ( fsLit "GET_ENTRY",    \ [x] -> entryCode dflags (closureInfoPtr dflags x) ),
  ( fsLit "GET_STD_INFO", \ [x] -> infoTable dflags (closureInfoPtr dflags x) ),
  ( fsLit "GET_FUN_INFO", \ [x] -> funInfoTable dflags (closureInfoPtr dflags x) ),
  ( fsLit "INFO_TYPE",    \ [x] -> infoTableClosureType dflags x ),
  ( fsLit "INFO_PTRS",    \ [x] -> infoTablePtrs dflags x ),
  ( fsLit "INFO_NPTRS",   \ [x] -> infoTableNonPtrs dflags x )
  ]

-- we understand a subset of C-- primitives:
machOps = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "add",        MO_Add ),
        ( "sub",        MO_Sub ),
        ( "eq",         MO_Eq ),
        ( "ne",         MO_Ne ),
        ( "mul",        MO_Mul ),
        ( "neg",        MO_S_Neg ),
        ( "quot",       MO_S_Quot ),
        ( "rem",        MO_S_Rem ),
        ( "divu",       MO_U_Quot ),
        ( "modu",       MO_U_Rem ),

        ( "ge",         MO_S_Ge ),
        ( "le",         MO_S_Le ),
        ( "gt",         MO_S_Gt ),
        ( "lt",         MO_S_Lt ),

        ( "geu",        MO_U_Ge ),
        ( "leu",        MO_U_Le ),
        ( "gtu",        MO_U_Gt ),
        ( "ltu",        MO_U_Lt ),

        ( "and",        MO_And ),
        ( "or",         MO_Or ),
        ( "xor",        MO_Xor ),
        ( "com",        MO_Not ),
        ( "shl",        MO_Shl ),
        ( "shrl",       MO_U_Shr ),
        ( "shra",       MO_S_Shr ),

        ( "fadd",       MO_F_Add ),
        ( "fsub",       MO_F_Sub ),
        ( "fneg",       MO_F_Neg ),
        ( "fmul",       MO_F_Mul ),
        ( "fquot",      MO_F_Quot ),

        ( "feq",        MO_F_Eq ),
        ( "fne",        MO_F_Ne ),
        ( "fge",        MO_F_Ge ),
        ( "fle",        MO_F_Le ),
        ( "fgt",        MO_F_Gt ),
        ( "flt",        MO_F_Lt ),

        ( "lobits8",  flip MO_UU_Conv W8  ),
        ( "lobits16", flip MO_UU_Conv W16 ),
        ( "lobits32", flip MO_UU_Conv W32 ),
        ( "lobits64", flip MO_UU_Conv W64 ),

        ( "zx16",     flip MO_UU_Conv W16 ),
        ( "zx32",     flip MO_UU_Conv W32 ),
        ( "zx64",     flip MO_UU_Conv W64 ),

        ( "sx16",     flip MO_SS_Conv W16 ),
        ( "sx32",     flip MO_SS_Conv W32 ),
        ( "sx64",     flip MO_SS_Conv W64 ),

        ( "f2f32",    flip MO_FF_Conv W32 ),  -- TODO; rounding mode
        ( "f2f64",    flip MO_FF_Conv W64 ),  -- TODO; rounding mode
        ( "f2i8",     flip MO_FS_Conv W8 ),
        ( "f2i16",    flip MO_FS_Conv W16 ),
        ( "f2i32",    flip MO_FS_Conv W32 ),
        ( "f2i64",    flip MO_FS_Conv W64 ),
        ( "i2f32",    flip MO_SF_Conv W32 ),
        ( "i2f64",    flip MO_SF_Conv W64 )
        ]

callishMachOps = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "write_barrier", MO_WriteBarrier ),
        ( "memcpy", MO_Memcpy ),
        ( "memset", MO_Memset ),
        ( "memmove", MO_Memmove ),

        ("prefetch0",MO_Prefetch_Data 0),
        ("prefetch1",MO_Prefetch_Data 1),
        ("prefetch2",MO_Prefetch_Data 2),
        ("prefetch3",MO_Prefetch_Data 3)

        -- ToDo: the rest, maybe
        -- edit: which rest?
        -- also: how do we tell CMM Lint how to type check callish macops?
    ]

parseSafety :: String -> P Safety
parseSafety "safe"   = return PlaySafe
parseSafety "unsafe" = return PlayRisky
parseSafety "interruptible" = return PlayInterruptible
parseSafety str      = fail ("unrecognised safety: " ++ str)

parseCmmHint :: String -> P ForeignHint
parseCmmHint "ptr"    = return AddrHint
parseCmmHint "signed" = return SignedHint
parseCmmHint str      = fail ("unrecognised hint: " ++ str)

-- labels are always pointers, so we might as well infer the hint
inferCmmHint :: CmmExpr -> ForeignHint
inferCmmHint (CmmLit (CmmLabel _)) = AddrHint
inferCmmHint (CmmReg (CmmGlobal g)) | isPtrGlobalReg g = AddrHint
inferCmmHint _ = NoHint

isPtrGlobalReg Sp                    = True
isPtrGlobalReg SpLim                 = True
isPtrGlobalReg Hp                    = True
isPtrGlobalReg HpLim                 = True
isPtrGlobalReg CCCS                  = True
isPtrGlobalReg CurrentTSO            = True
isPtrGlobalReg CurrentNursery        = True
isPtrGlobalReg (VanillaReg _ VGcPtr) = True
isPtrGlobalReg _                     = False

happyError :: P a
happyError = srcParseFail

-- -----------------------------------------------------------------------------
-- Statement-level macros

stmtMacro :: FastString -> [CmmParse CmmExpr] -> P (CmmParse ())
stmtMacro fun args_code = do
  case lookupUFM stmtMacros fun of
    Nothing -> fail ("unknown macro: " ++ unpackFS fun)
    Just fcode -> return $ do
        args <- sequence args_code
        code (fcode args)

stmtMacros :: UniqFM ([CmmExpr] -> FCode ())
stmtMacros = listToUFM [
  ( fsLit "CCS_ALLOC",             \[words,ccs]  -> profAlloc words ccs ),
  ( fsLit "ENTER_CCS_THUNK",       \[e] -> enterCostCentreThunk e ),

  ( fsLit "CLOSE_NURSERY",         \[]  -> emitCloseNursery ),
  ( fsLit "OPEN_NURSERY",          \[]  -> emitOpenNursery ),

  -- completely generic heap and stack checks, for use in high-level cmm.
  ( fsLit "HP_CHK_GEN",            \[bytes] ->
                                      heapStackCheckGen Nothing (Just bytes) ),
  ( fsLit "STK_CHK_GEN",           \[] ->
                                      heapStackCheckGen (Just (CmmLit CmmHighStackMark)) Nothing ),

  -- A stack check for a fixed amount of stack.  Sounds a bit strange, but
  -- we use the stack for a bit of temporary storage in a couple of primops
  ( fsLit "STK_CHK_GEN_N",         \[bytes] ->
                                      heapStackCheckGen (Just bytes) Nothing ),

  -- A stack check on entry to a thunk, where the argument is the thunk pointer.
  ( fsLit "STK_CHK_NP"   ,         \[node] -> entryHeapCheck' False node 0 [] (return ())),

  ( fsLit "LOAD_THREAD_STATE",     \[] -> emitLoadThreadState ),
  ( fsLit "SAVE_THREAD_STATE",     \[] -> emitSaveThreadState ),

  ( fsLit "LDV_ENTER",             \[e] -> ldvEnter e ),
  ( fsLit "LDV_RECORD_CREATE",     \[e] -> ldvRecordCreate e ),

  ( fsLit "PUSH_UPD_FRAME",        \[sp,e] -> emitPushUpdateFrame sp e ),
  ( fsLit "SET_HDR",               \[ptr,info,ccs] ->
                                        emitSetDynHdr ptr info ccs ),
  ( fsLit "TICK_ALLOC_PRIM",       \[hdr,goods,slop] ->
                                        tickyAllocPrim hdr goods slop ),
  ( fsLit "TICK_ALLOC_PAP",        \[goods,slop] ->
                                        tickyAllocPAP goods slop ),
  ( fsLit "TICK_ALLOC_UP_THK",     \[goods,slop] ->
                                        tickyAllocThunk goods slop ),
  ( fsLit "UPD_BH_UPDATABLE",      \[reg] -> emitBlackHoleCode reg )
 ]

emitPushUpdateFrame :: CmmExpr -> CmmExpr -> FCode ()
emitPushUpdateFrame sp e = do
  dflags <- getDynFlags
  emitUpdateFrame dflags sp mkUpdInfoLabel e

pushStackFrame :: [CmmParse CmmExpr] -> CmmParse () -> CmmParse ()
pushStackFrame fields body = do
  dflags <- getDynFlags
  exprs <- sequence fields
  updfr_off <- getUpdFrameOff
  let (new_updfr_off, _, g) = copyOutOflow dflags NativeReturn Ret Old
                                           [] updfr_off exprs
  emit g
  withUpdFrameOff new_updfr_off body

reserveStackFrame
  :: CmmParse CmmExpr
  -> CmmParse CmmReg
  -> CmmParse ()
  -> CmmParse ()
reserveStackFrame psize preg body = do
  dflags <- getDynFlags
  old_updfr_off <- getUpdFrameOff
  reg <- preg
  esize <- psize
  let size = case constantFoldExpr dflags esize of
               CmmLit (CmmInt n _) -> n
               _other -> pprPanic "CmmParse: not a compile-time integer: "
                            (ppr esize)
  let frame = old_updfr_off + wORD_SIZE dflags * fromIntegral size
  emitAssign reg (CmmStackSlot Old frame)
  withUpdFrameOff frame body

profilingInfo dflags desc_str ty_str
  = if not (gopt Opt_SccProfilingOn dflags)
    then NoProfilingInfo
    else ProfilingInfo (stringToWord8s desc_str)
                       (stringToWord8s ty_str)

staticClosure :: PackageId -> FastString -> FastString -> [CmmLit] -> CmmParse ()
staticClosure pkg cl_label info payload
  = do dflags <- getDynFlags
       let lits = mkStaticClosure dflags (mkCmmInfoLabel pkg info) dontCareCCS payload [] [] []
       code $ emitDataLits (mkCmmDataLabel pkg cl_label) lits

foreignCall
        :: String
        -> [CmmParse (LocalReg, ForeignHint)]
        -> CmmParse CmmExpr
        -> [CmmParse (CmmExpr, ForeignHint)]
        -> Safety
        -> CmmReturnInfo
        -> P (CmmParse ())
foreignCall conv_string results_code expr_code args_code safety ret
  = do  conv <- case conv_string of
          "C" -> return CCallConv
          "stdcall" -> return StdCallConv
          _ -> fail ("unknown calling convention: " ++ conv_string)
        return $ do
          dflags <- getDynFlags
          results <- sequence results_code
          expr <- expr_code
          args <- sequence args_code
          let
                  expr' = adjCallTarget dflags conv expr args
                  (arg_exprs, arg_hints) = unzip args
                  (res_regs,  res_hints) = unzip results
                  fc = ForeignConvention conv arg_hints res_hints ret
                  target = ForeignTarget expr' fc
          _ <- code $ emitForeignCall safety res_regs target arg_exprs
          return ()


doReturn :: [CmmParse CmmExpr] -> CmmParse ()
doReturn exprs_code = do
  dflags <- getDynFlags
  exprs <- sequence exprs_code
  updfr_off <- getUpdFrameOff
  emit (mkReturnSimple dflags exprs updfr_off)

mkReturnSimple  :: DynFlags -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple dflags actuals updfr_off =
  mkReturn dflags e actuals updfr_off
  where e = entryCode dflags (CmmLoad (CmmStackSlot Old updfr_off)
                             (gcWord dflags))

doRawJump :: CmmParse CmmExpr -> [GlobalReg] -> CmmParse ()
doRawJump expr_code vols = do
  dflags <- getDynFlags
  expr <- expr_code
  updfr_off <- getUpdFrameOff
  emit (mkRawJump dflags expr updfr_off vols)

doJumpWithStack :: CmmParse CmmExpr -> [CmmParse CmmExpr]
                -> [CmmParse CmmExpr] -> CmmParse ()
doJumpWithStack expr_code stk_code args_code = do
  dflags <- getDynFlags
  expr <- expr_code
  stk_args <- sequence stk_code
  args <- sequence args_code
  updfr_off <- getUpdFrameOff
  emit (mkJumpExtra dflags NativeNodeCall expr args updfr_off stk_args)

doCall :: CmmParse CmmExpr -> [CmmParse LocalReg] -> [CmmParse CmmExpr]
       -> CmmParse ()
doCall expr_code res_code args_code = do
  dflags <- getDynFlags
  expr <- expr_code
  args <- sequence args_code
  ress <- sequence res_code
  updfr_off <- getUpdFrameOff
  c <- code $ mkCall expr (NativeNodeCall,NativeReturn) ress args updfr_off []
  emit c

adjCallTarget :: DynFlags -> CCallConv -> CmmExpr -> [(CmmExpr, ForeignHint) ]
              -> CmmExpr
-- On Windows, we have to add the '@N' suffix to the label when making
-- a call with the stdcall calling convention.
adjCallTarget dflags StdCallConv (CmmLit (CmmLabel lbl)) args
 | platformOS (targetPlatform dflags) == OSMinGW32
  = CmmLit (CmmLabel (addLabelSize lbl (sum (map size args))))
  where size (e, _) = max (wORD_SIZE dflags) (widthInBytes (typeWidth (cmmExprType dflags e)))
                 -- c.f. CgForeignCall.emitForeignCall
adjCallTarget _ _ expr _
  = expr

primCall
        :: [CmmParse (CmmFormal, ForeignHint)]
        -> FastString
        -> [CmmParse CmmExpr]
        -> P (CmmParse ())
primCall results_code name args_code
  = case lookupUFM callishMachOps name of
        Nothing -> fail ("unknown primitive " ++ unpackFS name)
        Just p  -> return $ do
                results <- sequence results_code
                args <- sequence args_code
                code (emitPrimCall (map fst results) p args)

doStore :: CmmType -> CmmParse CmmExpr  -> CmmParse CmmExpr -> CmmParse ()
doStore rep addr_code val_code
  = do dflags <- getDynFlags
       addr <- addr_code
       val <- val_code
        -- if the specified store type does not match the type of the expr
        -- on the rhs, then we insert a coercion that will cause the type
        -- mismatch to be flagged by cmm-lint.  If we don't do this, then
        -- the store will happen at the wrong type, and the error will not
        -- be noticed.
       let val_width = typeWidth (cmmExprType dflags val)
           rep_width = typeWidth rep
       let coerce_val
                | val_width /= rep_width = CmmMachOp (MO_UU_Conv val_width rep_width) [val]
                | otherwise              = val
       emitStore addr coerce_val

-- -----------------------------------------------------------------------------
-- If-then-else and boolean expressions

data BoolExpr
  = BoolExpr `BoolAnd` BoolExpr
  | BoolExpr `BoolOr`  BoolExpr
  | BoolNot BoolExpr
  | BoolTest CmmExpr

-- ToDo: smart constructors which simplify the boolean expression.

cmmIfThenElse cond then_part else_part = do
     then_id <- newBlockId
     join_id <- newBlockId
     c <- cond
     emitCond c then_id
     else_part
     emit (mkBranch join_id)
     emitLabel then_id
     then_part
     -- fall through to join
     emitLabel join_id

cmmRawIf cond then_id = do
    c <- cond
    emitCond c then_id

-- 'emitCond cond true_id'  emits code to test whether the cond is true,
-- branching to true_id if so, and falling through otherwise.
emitCond (BoolTest e) then_id = do
  else_id <- newBlockId
  emit (mkCbranch e then_id else_id)
  emitLabel else_id
emitCond (BoolNot (BoolTest (CmmMachOp op args))) then_id
  | Just op' <- maybeInvertComparison op
  = emitCond (BoolTest (CmmMachOp op' args)) then_id
emitCond (BoolNot e) then_id = do
  else_id <- newBlockId
  emitCond e else_id
  emit (mkBranch then_id)
  emitLabel else_id
emitCond (e1 `BoolOr` e2) then_id = do
  emitCond e1 then_id
  emitCond e2 then_id
emitCond (e1 `BoolAnd` e2) then_id = do
        -- we'd like to invert one of the conditionals here to avoid an
        -- extra branch instruction, but we can't use maybeInvertComparison
        -- here because we can't look too closely at the expression since
        -- we're in a loop.
  and_id <- newBlockId
  else_id <- newBlockId
  emitCond e1 and_id
  emit (mkBranch else_id)
  emitLabel and_id
  emitCond e2 then_id
  emitLabel else_id


-- -----------------------------------------------------------------------------
-- Table jumps

-- We use a simplified form of C-- switch statements for now.  A
-- switch statement always compiles to a table jump.  Each arm can
-- specify a list of values (not ranges), and there can be a single
-- default branch.  The range of the table is given either by the
-- optional range on the switch (eg. switch [0..7] {...}), or by
-- the minimum/maximum values from the branches.

doSwitch :: Maybe (Int,Int) -> CmmParse CmmExpr -> [([Int],Either BlockId (CmmParse ()))]
         -> Maybe (CmmParse ()) -> CmmParse ()
doSwitch mb_range scrut arms deflt
   = do
        -- Compile code for the default branch
        dflt_entry <- 
                case deflt of
                  Nothing -> return Nothing
                  Just e  -> do b <- forkLabelledCode e; return (Just b)

        -- Compile each case branch
        table_entries <- mapM emitArm arms

        -- Construct the table
        let
            all_entries = concat table_entries
            ixs = map fst all_entries
            (min,max) 
                | Just (l,u) <- mb_range = (l,u)
                | otherwise              = (minimum ixs, maximum ixs)

            entries = elems (accumArray (\_ a -> Just a) dflt_entry (min,max)
                                all_entries)
        expr <- scrut
        -- ToDo: check for out of range and jump to default if necessary
        emit (mkSwitch expr entries)
   where
        emitArm :: ([Int],Either BlockId (CmmParse ())) -> CmmParse [(Int,BlockId)]
        emitArm (ints,Left blockid) = return [ (i,blockid) | i <- ints ]
        emitArm (ints,Right code) = do
           blockid <- forkLabelledCode code
           return [ (i,blockid) | i <- ints ]

forkLabelledCode :: CmmParse () -> CmmParse BlockId
forkLabelledCode p = do
  ag <- getCode p
  l <- newBlockId
  emitOutOfLine l ag
  return l

-- -----------------------------------------------------------------------------
-- Putting it all together

-- The initial environment: we define some constants that the compiler
-- knows about here.
initEnv :: DynFlags -> Env
initEnv dflags = listToUFM [
  ( fsLit "SIZEOF_StgHeader",
    VarN (CmmLit (CmmInt (fromIntegral (fixedHdrSize dflags * wORD_SIZE dflags)) (wordWidth dflags)) )),
  ( fsLit "SIZEOF_StgInfoTable",
    VarN (CmmLit (CmmInt (fromIntegral (stdInfoTableSizeB dflags)) (wordWidth dflags)) ))
  ]

parseCmmFile :: DynFlags -> FilePath -> IO (Messages, Maybe CmmGroup)
parseCmmFile dflags filename = do
  showPass dflags "ParseCmm"
  buf <- hGetStringBuffer filename
  let
        init_loc = mkRealSrcLoc (mkFastString filename) 1 1
        init_state = (mkPState dflags buf init_loc) { lex_state = [0] }
                -- reset the lex_state: the Lexer monad leaves some stuff
                -- in there we don't want.
  case unP cmmParse init_state of
    PFailed span err -> do
        let msg = mkPlainErrMsg dflags span err
        return ((emptyBag, unitBag msg), Nothing)
    POk pst code -> do
        st <- initC
        let (cmm,_) = runC dflags no_module st (getCmm (unEC code (initEnv dflags) [] >> return ()))
        let ms = getMessages pst
        if (errorsFound dflags ms)
         then return (ms, Nothing)
         else do
           dumpIfSet_dyn dflags Opt_D_dump_cmm "Cmm" (ppr cmm)
           return (ms, Just cmm)
  where
        no_module = panic "parseCmmFile: no module"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
