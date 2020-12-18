module Main where

import Control.Monad
import Language.Haskell.Interpreter
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  r <-
    runInterpreter $ do
      loadModules ["src/Lib.hs"]
      setTopLevelModules ["Lib"]
      setImports ["Prelude"]
      a <- eval $ transformExpr contents
      liftIO $ print a
  case r of
    Left err -> print err
    Right () -> return ()
