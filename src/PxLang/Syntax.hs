{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module PxLang.Syntax where

import           Control.Arrow                                ((>>>))
import           Control.Comonad.Cofree
import           Data.Char                                    (toLower)
import           Data.Eq.Deriving                             (deriveEq1)
import           Data.Fix
import           Data.Ord.Deriving                            (deriveOrd1)
import           Data.String                                  (IsString (..))
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP
import           Text.Show.Deriving                           (deriveShow1)


-- $setup
-- >>> import PxLang.Parser
-- >>> :set -XOverloadedStrings


newtype Name = Name { unName :: String } deriving (Show, Eq, Ord)


instance IsString Name where
  fromString = Name


data PrimOp = Add
            | Sub
            | Mult
            | Div
            | Equal
            | If
            deriving (Show, Eq, Ord, Enum, Bounded)


allPrimOps :: [PrimOp]
allPrimOps = [minBound ..]


opToName :: PrimOp -> Name
opToName = Name . ('#':) . map toLower . show


data Lit = LitInt Int
         | LitFloat Float
         | LitBool Bool
         deriving (Show, Eq, Ord)


data Expr a = Var Name
            | App a a
            | Lam Name a
            | Let Name a a
            | Lit Lit
            | Op PrimOp
            deriving (Show, Functor, Foldable, Traversable)
$(deriveEq1 ''Expr)
$(deriveOrd1 ''Expr)
$(deriveShow1 ''Expr)


instance Pretty Name where
  pPrint = PP.text . unName


instance Pretty Lit where
  pPrint (LitInt i)   = PP.int i
  pPrint (LitFloat f) = PP.float f
  pPrint (LitBool b)  = PP.text $ show b


instance Pretty PrimOp where
  pPrint = PP.text . show


subExprs :: Fix Expr -> [Fix Expr]
subExprs = unFix >>> \case Var _     -> []
                           Lit _     -> []
                           App a b   -> [a, b]
                           Let _ a b -> [a, b]
                           Lam _ a   -> [a]
                           Op _      -> []


isMultiExpr :: Fix Expr -> Bool
isMultiExpr = not . null . subExprs


fix2Cofree :: Functor f => (f a -> a) -> Fix f -> Cofree f a
fix2Cofree f e = cata f e :< fmap (fix2Cofree f) (unFix e)


fix2CofreeM
  :: (Functor f, Applicative m, Monad m, Traversable f)
  => (f a -> m a)
  -> Fix f
  -> m (Cofree f a)
fix2CofreeM f e = do
  a <- cataM f e
  g <- sequence $ fix2CofreeM f <$> unFix e
  return $ a :< g


cofree2Fix :: Functor f => Cofree f a -> Fix f
cofree2Fix = Fix . fmap cofree2Fix . unwrap


parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf = \case True -> PP.parens
                 _    -> id


arrowPrec :: Int
arrowPrec = 1


appPrec :: Int
appPrec = 2


prettyPrecSyntax :: Int -> Fix Expr -> PP.Doc
prettyPrecSyntax p = unFix >>> \case
  Var n -> pPrint n
  App a b -> parensIf (p > arrowPrec) $
    PP.hsep [prettyPrecSyntax arrowPrec a, prettyPrecSyntax appPrec b]
  Lam n a -> parensIf (p > 0) $
    PP.hsep [ PP.hcat [ PP.text "\\", pPrint n ]
            , PP.text "->"
            , prettyPrecSyntax 0 a
            ]
  Let n a b -> parensIf (p > 1) $
    PP.hsep [ PP.text "let"
            , pPrint n
            , PP.text "="
            , prettyPrecSyntax 0 a
            , PP.text "in"
            , prettyPrecSyntax 0 b
            ]
  Lit l -> pPrint l
  Op op -> PP.text $ unName $ opToName op



-- | Pretty expressions.
--
-- >>> :{
-- putStr $ pxPretty $ Fix $
--   Let "x" (Fix $ Lit $ LitFloat 20)
--           (Fix $ App (Fix $ App (Fix $ Var "equal")
--                                 (Fix $ Lit $ LitFloat 3)
--                      )
--                (Fix $ Var "x")
--           )
-- >>> :}
-- let x = 20.0 in equal 3.0 x
instance Pretty (Fix Expr) where
  pPrint = prettyPrecSyntax 0


prettyStr :: Pretty a => a -> String
prettyStr = PP.render . pPrint


pxPretty :: Fix Expr -> String
pxPretty = prettyStr
