{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PxLang.Rename where

import           Control.Arrow ((&&&))
import           Data.Fix      (Fix (..), cata)

import           PxLang.Syntax


-- $setup
-- >>> import PxLang.Parser


mayPrimOp :: Name -> Maybe PrimOp
mayPrimOp n = lookup n $ map (opToName &&& id) allPrimOps


-- | Rename an expression, reducing or expanding certain patterns into
--
-- >>> renameExpr <$> prettyReplParse "#add 0 1"
-- Right (Fix (App (Fix (App (Fix (Op Add)) (Fix (Lit (LitInt 0))))) (Fix (Lit (LitInt 1)))))
renameExpr :: Fix Expr -> Fix Expr
renameExpr = cata $ \case
  Var n | Just op <- mayPrimOp n -> Fix $ Op op
  e -> Fix e
