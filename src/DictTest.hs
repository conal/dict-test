-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  DictTest
-- Copyright   :  (c) 2014 Tabula, Inc.
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Test HERMIT-based dictionary construction
----------------------------------------------------------------------

module DictTest where

import Control.Category ((>>>))

import HERMIT.Dictionary hiding (externals) -- re-exports HERMIT.Dictionary.*
import HERMIT.External (External,ExternalName,external,(.+),CmdTag(Loop))
import HERMIT.GHC
import HERMIT.Kure
import HERMIT.Monad (saveDef,newIdH,Label)
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
