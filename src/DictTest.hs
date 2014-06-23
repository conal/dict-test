{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  DictTest
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test HERMIT-based dictionary construction
----------------------------------------------------------------------

-- #define MyBuildDict

module DictTest where

import Control.Category ((>>>))

import HERMIT.Dictionary hiding (externals
#ifdef MyBuildDict
                                          , buildDictionaryT
#endif
                                                            )
import HERMIT.External (External,external)
import HERMIT.GHC
import HERMIT.Kure
import HERMIT.Plugin (hermitPlugin,phase,interactive)

#ifdef MyBuildDict
-- For experimental buildDictionaryT tweaks
import Data.Char (isSpace)
import HERMIT.Monad (HermitM,liftCoreM,getModGuts,newIdH)
#endif

-- | e ==> succ e, if e has a type in Enum
succR :: RewriteH CoreExpr
succR = do e      <- idR
           let ty = exprType e
           succId <- findIdT "GHC.Enum.succ"
           enumTc <- findTyConT "GHC.Enum.Enum"
           dict   <- return (TyConApp enumTc [ty]) >>> buildDictionaryT
           return $ mkCoreApps (Var succId) [Type ty, dict, e]

externals :: [External]
externals = [external "succ" (promoteR succR :: RewriteH Core) ["..."]]

plugin :: Plugin
plugin = hermitPlugin (phase 0 . interactive externals)

#ifdef MyBuildDict
buildDictionaryT :: Transform c HermitM Type CoreExpr
buildDictionaryT = contextfreeT $ \ ty -> do
    dflags <- getDynFlags
    binder <- newIdH ("$d" ++ filter (not . isSpace) (showPpr dflags ty)) ty
    guts <- getModGuts
    (i,bnds) <- liftCoreM $ buildDictionary guts binder
    if null bnds then
      fail "couldn't build dictionary"
     else
       return $ case bnds of
                  [NonRec v e] | i == v -> e -- the common case that we would have gotten a single non-recursive let
                  _ -> mkCoreLets bnds (varToCoreExpr i)
#endif
