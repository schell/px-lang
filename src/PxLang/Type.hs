{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module PxLang.Type where


import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Monad.Except                         (ExceptT (..),
                                                               MonadError (..),
                                                               runExceptT)
import           Control.Monad.Identity                       (Identity,
                                                               runIdentity)
import           Control.Monad.Reader                         (MonadReader (..),
                                                               ask)
import           Control.Monad.RWS                            (RWST, runRWST)
import           Control.Monad.State                          (MonadState (..),
                                                               get, put, evalState, State)
import           Control.Monad.Writer                         (MonadWriter (..))
import           Data.Bifunctor
import           Data.Fix                                     (Fix (..))
import           Data.List                                    (delete, find)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as M
import           Data.Maybe                                   (fromMaybe)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as S
import           "prettyclass" Text.PrettyPrint.HughesPJClass (Pretty (..))
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP

import           PxLang.Parser
import           PxLang.Rename
import           PxLang.Syntax


-- $setup
-- >>> import PxLang.Parser
-- >>> import Text.Show.Pretty (ppShow)
-- >>> :set -XFlexibleContexts
-- >>> let epPrint = either pPrint pPrint

--------------------------------------------------------------------------------
-- Types and Schemes
--------------------------------------------------------------------------------
newtype TVar = TVar { unTVar :: Int } deriving (Show, Eq, Ord)


instance Pretty TVar where
  pPrint = PP.text . (allNames !!) . unTVar


data Ty = TyVar TVar
        | TyCon String
        | TyArr Ty Ty
        deriving (Show, Eq, Ord)


isArrowTy :: Ty -> Bool
isArrowTy TyArr{} = True
isArrowTy _       = False


instance Pretty Ty where
  pPrint = \case
    TyVar v   -> pPrint v
    TyCon s   -> PP.text s
    TyArr a b -> PP.hsep [ parensIf (isArrowTy a) $ pPrint a
                         , PP.text "->"
                         , pPrint b
                         ]


-- | Type arrow.
--
-- >>> :{
-- let tya = tyBool --> tyFloat --> tyInt
--     tyb = tyBool --> (tyFloat --> tyInt)
-- in tya == tyb
-- >>> :}
-- True
(-->) :: Ty -> Ty -> Ty
(-->) = TyArr
infixr 1 -->


tyBool :: Ty
tyBool = TyCon "Bool"


tyInt :: Ty
tyInt = TyCon "Int"


tyFloat :: Ty
tyFloat = TyCon "Float"


data Scheme = Forall [TVar] Ty
  deriving (Show, Eq, Ord)


newtype TypeEnv = TypeEnv { unTypeEnv :: Map Name Scheme }


extendTypeEnv :: TypeEnv -> Name -> Scheme -> TypeEnv
extendTypeEnv env n sch = TypeEnv $ M.insert n sch $ unTypeEnv env


--------------------------------------------------------------------------------
-- | A cofree approach to generating type constraints
-- https://brianmckenna.org/blog/type_annotation_cofree
--------------------------------------------------------------------------------


newtype Assumptions = Assumptions { unAssumptions :: [(Name, Ty)] }
  deriving (Eq, Show, Semigroup, Monoid)


extendAssumptions :: Assumptions -> (Name, Ty) -> Assumptions
extendAssumptions (Assumptions a) (x, s) = Assumptions ((x, s) : a)


removeAssumption :: Name -> Assumptions -> Assumptions
removeAssumption var (Assumptions a) =
  Assumptions (filter (\(n, _) -> n /= var) a)


findAssumedTypes :: Name -> Assumptions -> [Ty]
findAssumedTypes key (Assumptions a) = map snd (filter (\(n, _) -> n == key) a)


singleAssumption :: Name -> Ty -> Assumptions
singleAssumption x y = Assumptions[(x, y)]


namedAssumptions :: Assumptions -> [Name]
namedAssumptions (Assumptions a) = map fst a


-- | Represents constraints within our type system.
-- The first is an equality constraint the other two are used to cope with
-- polymorphism that is introduced by let-expressions.
data Constraint
  = ConstraintEq Ty Ty
  -- ^ An equality constraint reflects that t1 and t2 should be unified at a
  -- later stage of the type inferencing process.
  | ConstraintExpInst Ty Scheme
  -- ^ An explicit instance constraint.
  -- States that the type has to be a generic instance of the scheme. This
  -- constraint is convenient if we know the type scheme before we start type
  -- inferencing an expression. In general the (polymorphic) type of a
  -- declaration in a let-expression is unknown and must be inferred before it
  -- can be instantiated.
  | ConstraintImpInst Ty (Set TVar) Ty
  -- ^ An implicit instance constraint.
  -- Expresses that t1 should be an instance of the type scheme that is obtained
  -- by generalizing type t2 with respect to the set of monomorphic type
  -- variables M, i.e., quantifying over the polymorphic type variables.
  deriving (Eq, Ord, Show)


instance Pretty Constraint where
  pPrint = \case
    ConstraintEq a b -> PP.hsep [PP.text "ConstraintEq", pPrint a, pPrint b]
    ConstraintExpInst t s -> PP.hsep [PP.text "ConstraintExpInst", pPrint t, pPrint s]
    ConstraintImpInst a s b -> PP.hsep [PP.text "ConstraintImpInst", pPrint a, pPrint s, pPrint b]


instance Pretty a => Pretty (Set a) where
  pPrint s = PP.hsep [ PP.text "Set.fromList"
                     , pPrint $ S.toList s
                     ]


instance Pretty Scheme where
  pPrint (Forall as t) = PP.hsep [ PP.text "forall"
                                 , PP.hcat [PP.hsep $ map pPrint as, PP.text "."]
                                 , pPrint t
                                 ]



type Constraints = [Constraint]


instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pPrint m = PP.hsep [ PP.text "Map.fromList"
                     , pPrint $ M.toList m
                     ]


--data TyState t c = TyState { tyStateVarId   :: Int
--                           , tyStateMemoMap :: Map t c
--                           }


newtype Fresh = Fresh { unFresh :: Int } deriving (Show, Eq, Ord, Num)


--emptyTyState :: Ord t => TyState t c
--emptyTyState = TyState { tyStateMemoMap = mempty
--                       , tyStateVarId   = 0
--                       }


data TypeError = UnificationFailure Ty Ty
               | InfiniteType TVar Ty
               deriving (Show, Eq)


instance Pretty TypeError where
  pPrint = \case
    UnificationFailure a b -> PP.hsep [ PP.text "Could not unify type"
                                      , pPrint a
                                      , PP.text "with"
                                      , pPrint b
                                      ]
    InfiniteType tv t -> PP.hsep [ PP.text "Cannot construct the infinite type"
                                 , pPrint tv
                                 , PP.text "::"
                                 , pPrint t
                                 ]


data InferenceWarning = InferenceWarning String
                      | InferenceInfo String
                      deriving (Show, Eq)


type MonadInfer m = ( MonadReader (Set TVar) m
                    , MonadWriter [InferenceWarning] m
                    , MonadState Fresh m
                    , MonadError TypeError m
                    )


-- |
-- >>> :{
-- second fst $ runInfer $ do
--   v1 <- freshVar
--   extendMSet v1 $ do
--     v2 <- freshVar
--     return (v1, v2)
-- >>> :}
-- Right (TVar {unTVar = 0},TVar {unTVar = 1})
extendMSet :: MonadInfer m => TVar -> m a -> m a
extendMSet = local . S.insert


freshVar :: MonadInfer m => m TVar
freshVar = do
  Fresh k <- get
  tell [InferenceInfo $ "Alloc'd fresh var " ++ show k]
  put $ Fresh $ succ k
  return $ TVar k


freshTyVar :: MonadInfer m => m Ty
freshTyVar = TyVar <$> freshVar


--memoizedInfer :: (Ord t, MonadInfer t c m) => (t -> m c) -> t -> m c
--memoizedInfer f c = do
--  s <- get
--  case M.lookup c $ tyStateMemoMap s of
--    Just rs -> return rs
--    Nothing -> do
--      rs <- f c
--      put $ s{ tyStateMemoMap = M.insert c rs $ tyStateMemoMap s }
--      return rs


-- | All type variable names.
--
-- >>> pPrint $ take 4 allNames
-- ["a", "b", "c", "d"]
--
-- >>> pPrint $ take 2 $ drop 26 allNames
-- ["a1", "b1"]
allNames :: [String]
allNames =
  let alphas = map pure "abcdefghijklmnopqrstuvwxyz"
  in alphas ++ [ ch ++ show n
               | n  <- [1 :: Integer ..]
               , ch :: String <- cycle alphas
               ]


type InferT m =
  RWST (Set TVar)            -- Monomorphic set of type variables
       [InferenceWarning]    -- Warnings and info stuff
       Fresh                 -- Fresh vars R us
       (ExceptT TypeError m) -- Inference errors


runInferT
  :: Monad m
  => InferT m a
  -> Fresh
  -> Set TVar
  -> m (Either TypeError (a, Fresh, [InferenceWarning]))
runInferT f s m = runExceptT $ runRWST f m s


type Infer a = InferT Identity a


runInfer
  :: Infer a
  -> Either TypeError (a, [InferenceWarning])
runInfer f = do
  (a, _, ws) <- runIdentity $ runInferT f 0 mempty
  return (a, ws)


-- | Infer the Type, Assumptions and Constraints of an expression.
infer
  :: MonadInfer m
  => Cofree Expr x
  -> m (Ty, Assumptions, Constraints)
infer = generateConstraints


-- | Generate constraints according to
-- [The Bottom-Up Type Inference Rules](http://soft.vub.ac.be/~cfscholl/Capita-Selecta-2015/papers/2002%20Heeren.pdf#subsection.4.2)"
-- with help from:
-- * [Bottom Up Type Annotation with Cofree](https://brianmckenna.org/blog/type_annotation_cofree)
-- * [HM type inference with constraints](https://kseo.github.io/posts/2017-01-02-hindley-milner-inference-with-constraints.html)
generateConstraints
  :: MonadInfer m
  => Cofree Expr x
  -> m (Ty, Assumptions, Constraints)
generateConstraints = \case
 -- The inference rule for a variable is very straightforward:
 -- a fresh type variable β is introduced and returned as the type. We
 -- assume that at any time there are infinitely many fresh type variables
 -- available. The fact that β was assigned to the variable is recorded in the
 -- assumption set. The constraint set is empty.
  _ :< Var k -> do
    tell $ pure $ InferenceInfo $ unwords ["Var", unName k]
    var <- freshTyVar
    return (var, singleAssumption k var, mempty)
  -- Literals are just one type, no assumptions. Boom.
  _ :< Lit (LitBool _)  -> return (tyBool, mempty, mempty)
  _ :< Lit (LitFloat _) -> return (tyFloat, mempty, mempty)
  _ :< Lit (LitInt _)   -> return (tyInt, mempty, mempty)
  -- A new type variable β is introduced to represent the type of an
  -- application of two expressions. An equality constraint ensures that the
  -- domain and the range of the type of the first expression match with the
  -- type of the second expression and β respectively. Furthermore, the collected
  -- constraints for the subexpressions are passed on unchanged, and the two
  -- assumption sets are merged.
  _ :< App a b -> do
    tell $ pure $ InferenceInfo "App"
    var             <- freshTyVar
    (tya, asa, csa) <- infer a
    (tyb, asb, csb) <- infer b
    let tarr = tyb --> var
        asc  = asa <> asb
        csc  = csa <> csb <> [ConstraintEq tya tarr]
    return (var, asc, csc)
  -- The fresh β in the inference rule for a lambda abstraction represents the
  -- type of the lambda bound variable. An equality constraint is generated for
  -- each type variable in the assumption set that is associated with the
  -- variable that is bound by the lambda. The assumptions that concern this
  -- variable are removed from the assumption set.
  _ :< Lam k a -> do
    tell $ pure $ InferenceInfo $ unwords ["Lam", unName k]
    n <- freshVar
    let var = TyVar n
    (ty, as, cs) <- extendMSet n $ infer a
    return ( var --> ty
           , removeAssumption k as
           , cs <> [ ConstraintEq t var
                   | t <- findAssumedTypes k as
                   ]
           )
  -- Unsurprisingly, it is the let-expression that introduces polymorphism and
  -- brings in some difficulties. Inferring a type for a let-expression implies
  -- a specific order in which the types of the two subexpressions have to be
  -- computed. This order is reflected in the Hindley-Milner inference rules:
  -- the inferred type of the declaration is generalized before it is added to
  -- the type environment under which the type of the body is inferred. An
  -- implicit instance constraint is generated for each variable in the body
  -- that becomes bound. Although there is no order in the set of constraints,
  -- an implicit instance constraint requires some constraints to be solved
  -- before it becomes solvable.
  _ :< Let k a b -> do
    tell $ pure $ InferenceInfo $ unwords ["Let", unName k]
    (tya, asa, csa) <- infer a
    (tyb, asb, csb) <- infer b
    ms              <- ask
    return ( tyb
           , removeAssumption k $ asa <> asb
           , csa <> csb <> [ ConstraintImpInst t ms tya
                           | t <- findAssumedTypes k asb
                           ]
           )
  _ :< Op op -> do
    tell $ pure $ InferenceInfo $ unwords ["Op", show op]
    var <- freshTyVar
    let ty = case op of
               Add   -> var --> var --> var
               Sub   -> var --> var --> var
               Mult  -> var --> var --> var
               Div   -> tyFloat --> tyFloat --> tyFloat
               Equal -> var --> var --> tyBool
               If    -> tyBool --> var --> var --> var
    return (ty, mempty, mempty)


annotateConstraintsM
  :: MonadInfer m
  => Cofree Expr x
  -> m (Cofree Expr (Ty, Assumptions, Constraints))
annotateConstraintsM = sequence . extend generateConstraints


annotateConstraints
  :: Ord x
  => Cofree Expr x
  -> Either TypeError ( Cofree Expr (Ty, Assumptions, Constraints)
                      , [InferenceWarning]
                      )
annotateConstraints = runInfer . annotateConstraintsM


--------------------------------------------------------------------------------
-- Solving constraints
--------------------------------------------------------------------------------

-- | A map of type variables to types.
newtype Subst = Subst { unSubst :: Map TVar Ty }


emptySubst :: Subst
emptySubst = Subst mempty


composeSubst :: Subst -> Subst -> Subst
composeSubst (Subst s1) (Subst s2) =
  Subst $ M.union (M.map (subst $ Subst s1) s2) s1


instance Semigroup Subst where
  (<>) = composeSubst


instance Monoid Subst where
  mempty  = emptySubst
  mappend = (<>)


class Substitutable a where
  -- | Substitute @a@ with a map of substitutions.
  subst :: Subst -> a -> a


instance Substitutable Ty where
  -- A type constructor does not get substituted
  subst _ (TyCon a)     = TyCon a
  -- A type variable can be substituted with the associated type
  -- in the map of available substitutions. If nothing is found it
  -- defaults back to the original type variable.
  subst s t@(TyVar a)   = M.findWithDefault t a $ unSubst s
  -- An arrow can be substituted by substituting its type arguments.
  subst s (TyArr t1 t2) = TyArr (subst s t1) (subst s t2)



instance (Ord a, Substitutable a) => Substitutable (Set a) where
  subst = S.map . subst


instance Substitutable a => Substitutable [a] where
  subst = map . subst


instance Substitutable Constraint where
  subst s (ConstraintEq a b) = ConstraintEq (subst s a) (subst s b)
  subst s (ConstraintExpInst t sch) = ConstraintExpInst (subst s t) (subst s sch)
  subst s (ConstraintImpInst a ms b) = ConstraintImpInst (subst s a) (subst s ms) (subst s b)


instance Substitutable Scheme where
  subst (Subst s) (Forall as t) = let sub = Subst $ foldr M.delete s as
                                  in Forall as $ subst sub t


instance Substitutable TVar where
  subst s t = case M.findWithDefault (TyVar t) t $ unSubst s of
                TyVar tv -> tv
                _        -> t



class FreeTypeVars a where
  -- | Find all type variables in @a@.
  ftv :: a -> Set TVar


instance FreeTypeVars Ty where
  -- A constructor is fully concrete and has no type variables.
  ftv TyCon{}       = mempty
  -- A type variable ... has one type variable.
  ftv (TyVar a)     = S.singleton a
  -- A type arrow contains the union of its type arguments' variables.
  ftv (TyArr t1 t2) = ftv t1 <> ftv t2


instance FreeTypeVars TVar where
  ftv = S.singleton


instance FreeTypeVars a => FreeTypeVars (Set a) where
  ftv = foldr (S.union . ftv) mempty


instance FreeTypeVars Scheme where
  ftv (Forall as t) = S.difference (ftv t) $ S.fromList as


occursCheck :: TVar -> Ty -> Bool
occursCheck v = S.member v . ftv


-- | Bind a type variable to a type in a substitution.
bind :: MonadError TypeError m => TVar -> Ty -> m Subst
bind v t | TyVar v == t    = return mempty
         | occursCheck v t = throwError $ InfiniteType v t
         | otherwise       = return $ Subst $ M.singleton v t


-- | Unify two types.
-- This binds type variables to types within a substitution.
unify :: MonadError TypeError m => Ty -> Ty -> m Subst
unify a b | a == b = return mempty
unify (TyVar k) b = bind k b
unify a (TyVar k) = bind k a
unify (TyArr a b) (TyArr c d) = do
  subv <- unify a c
  suby <- unify (subst subv b) (subst subv d)
  return $ suby <> subv
unify a b = throwError $ UnificationFailure a b


-- | Extract the active type variables from a Constraint.
atv :: Constraint -> Set TVar
atv (ConstraintEq ta tb) = S.union (ftv ta) (ftv tb)
atv (ConstraintImpInst ta ms tb) =
  S.union (ftv ta) $ S.intersection (ftv ms) (ftv tb)
atv (ConstraintExpInst ta s) = S.union (ftv ta) (ftv s)


-- | Find the next solvable Constraint and return it separated from the rest of
-- the other Constraints. Requires at least one constraint.
nextSolvable :: Constraint -> [Constraint] -> (Constraint, [Constraint])
nextSolvable x xs = fromMaybe (x, []) $ find solvable $ choices (x:xs)
  where choices zs = [ (z, ys)
                     | z <- zs
                     , let ys = delete z zs
                     ]
        solvable (ConstraintEq _ _, _) = True
        solvable (ConstraintExpInst _ _, _) = True
        solvable (ConstraintImpInst _ ms tb, cs) =
          S.null $ S.intersection (S.difference ms $ ftv tb) (mconcat $ map atv cs)


solve
  :: forall m. MonadInfer m
  => [Constraint]
  -> m Subst
solve [] = return mempty
solve (x:xs) = uncurry subSolve $ nextSolvable x xs
  where subSolve :: Constraint -> [Constraint] -> m Subst
        subSolve (ConstraintEq ta tb) cs = do
          subh <- unify ta tb
          subt <- solve $ subst subh cs
          return $ subt <> subh
        subSolve (ConstraintImpInst ta ms tb) cs = do
          let c = ConstraintExpInst ta $ generalize ms tb
          subSolve c cs
        subSolve (ConstraintExpInst ta s) cs = do
          c <- ConstraintEq ta <$> instantiate s
          subSolve c cs
        generalize :: Set TVar -> Ty -> Scheme
        generalize free t = Forall (S.toList $ S.difference (ftv t) free) t
        instantiate :: Scheme -> m Ty
        instantiate (Forall as t) = do
          bs <- mapM (const freshTyVar) as
          let s = Subst $ M.fromList $ zip as bs
          return $ subst s t


--------------------------------------------------------------------------------
-- Magical HM
--------------------------------------------------------------------------------
data PxError = PxParseError String
             | PxTypeError TypeError
             deriving (Show, Eq)


instance Pretty PxError where
  pPrint = \case PxParseError err -> PP.text err
                 PxTypeError terr -> pPrint terr


liftParse :: Either String a -> Either PxError a
liftParse = first PxParseError


liftInfer :: Either TypeError a -> Either PxError a
liftInfer = first PxTypeError


annotateTypeM
  :: MonadInfer m
  => Cofree Expr x
  -> m (Cofree Expr Ty)
annotateTypeM coexpr = do
  coinfer@((_, _, cs) :< _) <- annotateConstraintsM coexpr
  subs                      <- solve cs
  return $ fmap (\(ty, _, _) -> subst subs ty) coinfer


annotateType :: Ord x => Cofree Expr x -> Either TypeError (Cofree Expr Ty, [InferenceWarning])
annotateType = runInfer . annotateTypeM


-- | Infer the type of the Fix Expr.
--
-- >>> :{
-- epPrint $ second (typeOf . fst) $ inferType
--    (Fix $ App (Fix $ App (Fix $ Op Add)
--                          (Fix $ Lit $ LitFloat 3.0)
--               )
--               (Fix $ Lit $ LitFloat 5.0)
--    )
-- >>> :}
-- Float
--
-- >>> parsePutPrettyType "\\x -> let y = x in y"
-- a -> a
--
-- >>> parsePutPrettyType "\\x -> x"
-- a -> a
--
-- >>> parsePutPrettyType "let x = x in x"
-- a
--
-- >>> parsePutPrettyType "let x = 0 in x"
-- Int
--
-- >>> parsePutPrettyType "\\x -> 0"
-- a -> Int
--
-- >>> parsePutPrettyType "let i = (\\x -> x) 0 in i"
-- Int
--
-- >>> parsePutPrettyType "\\x -> 0"
-- a -> Int
--
-- >>> parsePutPrettyType "let i = \\x -> 0 in i"
-- a -> Int
--
-- >>> parsePutPrettyType "let i = (\\x -> x) in i"
-- a -> a
--
-- >>> parsePutPrettyType "let id = \\x -> let y = x in y in id id"
-- a -> a
--
-- >>> parsePutPrettyType "(\\x y -> #add x y) True 6"
-- Could not unify type Bool with Int
inferType :: Fix Expr -> Either PxError (Cofree Expr Ty, [InferenceWarning])
inferType = liftInfer . annotateType . fix2Cofree (const ())


parseInfer :: String -> Either PxError (Cofree Expr Ty, [InferenceWarning])
parseInfer s = do
  e <- liftParse $ prettyReplParse s
  inferType $ renameExpr e


parseConstraints :: String -> Either PxError (Cofree Expr (Ty, Assumptions, Constraints), [InferenceWarning])
parseConstraints s = do
  e <- liftParse $ prettyReplParse s
  liftInfer $ annotateConstraints $ fix2Cofree (const ()) e


onlyAnnotation :: Cofree f a -> a
onlyAnnotation (a :< _) = a


typeOf :: Cofree Expr Ty -> Ty
typeOf = onlyAnnotation


-- | Renames a type to contain type variables grabbed from the top down.
--
-- >>> :{
-- let a = TyVar $ TVar 66
--     b = TyVar $ TVar 67
--     ty = a --> b
-- in PP.render $ pPrint $ renameType ty
-- >>> :}
-- "a -> b"
renameType :: Ty -> Ty
renameType = flip evalState (M.empty, 0) . go
  where
    go :: Ty -> State (Map Int Int, Int) Ty
    go (TyVar (TVar n)) = do
      (ts, k) <- get
      case M.lookup n ts of
        Just t -> return $ TyVar $ TVar t
        Nothing -> do
          put (M.insert n k ts, succ k)
          return $ TyVar $ TVar k
    go (TyArr t1 t2) =
      TyArr
        <$> go t1
        <*> go t2
    go ty = return ty


--
parsePutPrettyType :: String -> IO ()
parsePutPrettyType =
  either pp pp
  . second (renameType . typeOf . fst)
  . parseInfer
  where pp :: Pretty a => a -> IO ()
        pp = putStrLn . PP.render . pPrint
