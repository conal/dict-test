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

module DictTest where

import Control.Category ((>>>))

import HERMIT.Dictionary hiding (externals)
import HERMIT.External (External,external)
import HERMIT.GHC
import HERMIT.Kure
import HERMIT.Plugin (hermitPlugin,phase,interactive)

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
