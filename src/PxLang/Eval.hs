{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module PxLang.Eval where

import           Control.Arrow          ((>>>))
import           Control.Monad          (msum, void, when)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), asks)
import           Data.Fix
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Semigroup         (Semigroup)
import           Text.Show.Pretty       (ppShow)

import           PxLang.Syntax


newtype TermEnv = TermEnv { unTermEnv :: Map String (Fix Expr) }
                deriving (Show, Semigroup, Monoid)


showScope :: TermEnv -> String
showScope (TermEnv env) =
  let hdrs = "scope: " : repeat "       "
  in unlines $ zipWith (++) hdrs $ lines $ ppShow $ pxPretty <$> env


data Repl = Repl { replTermEnv     :: TermEnv
                 , replEvalDepth   :: Int
                 , replBreakOnEval :: Bool
                 , replParseOnly   :: Bool
                 } deriving (Show)


defaultRepl :: Repl
defaultRepl = Repl { replTermEnv     = mempty
                   , replEvalDepth   = 0
                   , replBreakOnEval = False
                   , replParseOnly   = False
                   }


extendEnvAnd
  :: (MonadError String m, MonadReader Repl m, MonadIO m)
  => Name
  -> Fix Expr
  -> m a
  -> m a
extendEnvAnd (Name name) expr =
  local $ \repl -> repl{ replTermEnv = TermEnv $ M.insert name expr
                                               $ unTermEnv
                                               $ replTermEnv repl
                       }


lookupTerm :: MonadReader Repl m => Name -> m (Maybe (Fix Expr))
lookupTerm (Name name) = M.lookup name . unTermEnv <$> asks replTermEnv


printTerms :: (MonadReader Repl m, MonadIO m) => m ()
printTerms = asks replTermEnv >>= liftIO . putStrLn . showScope


mayInt :: Fix Expr -> Maybe Int
mayInt (Fix (Lit (LitInt i))) = Just i
mayInt _                      = Nothing


mayFloat :: Fix Expr -> Maybe Float
mayFloat (Fix (Lit (LitFloat i))) = Just i
mayFloat _                        = Nothing


mayBool :: Fix Expr -> Maybe Bool
mayBool (Fix (Lit (LitBool i))) = Just i
mayBool _                       = Nothing


intOrFloatLit :: Maybe Int -> Maybe Float -> Maybe (Fix Expr)
intOrFloatLit i f = Fix . Lit <$> msum [LitInt <$> i, LitFloat <$> f]


mayLit :: Fix Expr -> Maybe Lit
mayLit (Fix (Lit l)) = Just l
mayLit _             = Nothing


--
--
--binfOp
--  :: MonadError String m
--  => BinOp
--  -> Fix Expr
--  -> Fix Expr
--  -> m (Fix Expr)
--binfOp op (Fix (Lit (LitFloat x))) (Fix (Lit (LitFloat y))) =
--  return $ Fix $ Lit $ op2Op op
--  where op2Op Add   = LitFloat $ x + y
--        op2Op Sub   = LitFloat $ x - y
--        op2Op Mult  = LitFloat $ x * y
--        op2Op Div   = LitFloat $ x / y
--        op2Op Equal = LitBool  $ x == y
--binfOp _ _ _ = throwError "Expected two Float arguments"
--
--
--boolOp
--  :: MonadError String m
--  => BinOp
--  -> Fix Expr
--  -> Fix Expr
--  -> m (Fix Expr)
--boolOp Equal (Fix (Lit x)) (Fix (Lit y)) =
--  return $ Fix $ Lit $ LitBool $ x == y
--boolOp Equal _ _ = throwError "Expected two like arguments"
--boolOp op x y = throwError $ unwords [ "Unsupported Bool operation "
--                                     , pxPretty (Fix $ Op op x y)
--                                     ]


sub :: Name -> Fix Expr -> Fix Expr -> Fix Expr
sub key value = unFix >>> \case
  Lit l -> Fix $ Lit l
  Let lhs rhs body -> Fix $ Let lhs (sub key value rhs)
                                    $ if lhs == key
                                      then body
                                      else sub key value body
  App x y -> Fix $ App (sub key value x) (sub key value y)
  Lam var body | var == key -> Fix $ Lam var body -- name shadowing
               | otherwise  -> Fix $ Lam var (sub key value body)
  Var var | var == key -> value
          | otherwise  -> Fix $ Var var
  Op op -> Fix $ Op op


incEvalDepth :: MonadReader Repl m => m a -> m a
incEvalDepth = local $ \repl -> repl{ replEvalDepth = succ $ replEvalDepth repl }


eval :: (MonadError String m, MonadReader Repl m, MonadIO m) => Fix Expr -> m (Fix Expr)
eval e = incEvalDepth $ do
  shouldBreak <- asks replBreakOnEval
  env         <- asks replTermEnv
  depth       <- asks replEvalDepth
  when shouldBreak $ do
    let padding = concat $ replicate depth "  "
    liftIO $ putStrLn
           $ unlines
           $ map (padding ++)
           $ concat [ lines (ppShow e)
                    , lines (showScope env)
                    , ["..."]
                    ]
    void $ liftIO getLine
  case unFix e of

    Var name -> do
      let n = unName name
      case M.lookup n $ unTermEnv env of
        Nothing -> throwError $ unlines [ unwords ["Term", show n, "is not in scope."]
                                        ]
        Just x  -> eval x

    App (Fix (App (Fix (Op Add)) a)) b -> do
      x <- eval a
      y <- eval b
      let int = (+) <$> mayInt x   <*> mayInt y
          flt = (+) <$> mayFloat x <*> mayFloat y
          mexp = intOrFloatLit int flt
          err = unwords ["Could not add", pxPretty x, "and", pxPretty y]
      maybe (throwError err) return mexp

    App (Fix (App (Fix (Op Sub)) a)) b -> do
      x <- eval a
      y <- eval b
      let int = (-) <$> mayInt x   <*> mayInt y
          flt = (-) <$> mayFloat x <*> mayFloat y
          mexp = intOrFloatLit int flt
          err = unwords ["Could not subtract", pxPretty x, "and", pxPretty y]
      maybe (throwError err) return mexp

    App (Fix (App (Fix (Op Mult)) a)) b -> do
      x <- eval a
      y <- eval b
      let int = (*) <$> mayInt x <*> mayInt y
          flt = (*) <$> mayFloat x <*> mayFloat y
          mexp = msum [ Fix . Lit . LitInt   <$> int
                      , Fix . Lit . LitFloat <$> flt
                      ]
          err = unwords ["Could not multiply", pxPretty x, "and", pxPretty y]
      maybe (throwError err) return mexp

    App (Fix (App (Fix (Op Div)) a)) b -> do
      x <- eval a
      y <- eval b
      let err = unwords ["Could not divide", pxPretty x, "and", pxPretty y]
          flt = (/) <$> mayFloat x <*> mayFloat y
          mexp = Fix . Lit . LitFloat <$> flt
      maybe (throwError err) return mexp

    app@(App (Fix (App (Fix (Op Equal)) a)) b) -> do
      x <- eval a
      y <- eval b
      let err = unwords ["Could not compute equality", show $ pxPretty $ Fix app]
          mlit = (==) <$> mayLit x <*> mayLit y
      maybe (throwError err) return $ Fix . Lit . LitBool <$> mlit

    app@(App (Fix (App (Fix (App (Fix (Op If)) i)) a)) b) -> do
      c <- eval i
      t <- eval a
      f <- eval b
      case mayBool c of
        Just True  -> return t
        Just False -> return f
        _ -> throwError $ unwords [ "Could not evaluate condition"
                                  , pxPretty $ Fix app
                                  ]

    app@(App f x) -> eval f >>= \case
      Fix (Lam n body) -> eval $ Fix $ Let n x body
      Fix (Op op) -> return $ Fix $ App (Fix $ Op op) x
      _ -> throwError $ unwords [ "Left hand side of application "
                                , show $ pxPretty $ Fix app
                                , "must be a lambda"
                                ]

    Let x y z -> do
      ey <- eval y
      eval $ sub x ey z

    expr -> return $ Fix expr
