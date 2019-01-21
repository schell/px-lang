{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
module PxLang.Repl where

import           Control.Monad.Except                         (MonadError,
                                                               liftEither,
                                                               runExceptT)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Control.Monad.Reader                         (MonadReader,
                                                               asks, runReaderT)
import           Data.Bifunctor                               (first)
import           Data.Fix                                     (Fix (..))
import           System.Console.Haskeline
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PP
import           Text.Show.Pretty                             (ppShow)

import           PxLang.Eval
import           PxLang.Parser
import           PxLang.Rename
import           PxLang.Syntax
import           PxLang.Type


evalLine
  :: (MonadError String m, MonadReader Repl m, MonadIO m) => String -> m (Fix Expr)
evalLine ln = do
  let eStrOrAST = prettyReplParse ln
  ast               <- liftEither eStrOrAST
  let renamedAst = renameExpr ast
  (coast, _tywarns) <- liftEither $ first prettyStr $ inferType renamedAst
  b <- asks replParseOnly >>= \case
    True -> do
      liftIO $ putStrLn $ ppShow renamedAst
      return renamedAst
    False -> eval renamedAst
  liftIO $ putStrLn
         $ PP.render
         $ PP.hang (parensIf (isMultiExpr b) $ PP.pPrint b)
                   2
                   (PP.hsep [ PP.text "::", PP.pPrint $ typeOf coast])
  return b


evalLineIO :: String -> IO ()
evalLineIO ln = runExceptT (runReaderT (evalLine ln) defaultRepl) >>= \case
  Left err -> putStrLn err
  Right _  -> return ()


repl :: IO ()
repl = runInputT defaultSettings $ loop defaultRepl
  where loop env = getInputLine "% " >>= \case
          Nothing   -> return ()
          Just ":q" -> return ()
          Just ":set breakOnEval"  -> loop $ env{ replBreakOnEval = True }
          Just ":set -breakOnEval" -> loop $ env{ replBreakOnEval = False }
          Just ":set parseOnly"    -> loop $ env{ replParseOnly = True }
          Just ":set -parseOnly"   -> loop $ env{ replParseOnly = False }
          Just ln  -> runExceptT (runReaderT (evalLine ln) env) >>= \case
            Left err -> outputStrLn err >> loop env
            Right _  -> loop env
