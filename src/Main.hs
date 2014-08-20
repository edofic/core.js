module Main where

import Immediate.Simplify (simplify)
import Immediate.Desugar (desugarModule)
import Immediate.GenerateJs (genModule)
import qualified Language.Core.Parser as P (parse) 
import qualified Language.Core.ParseGlue as PG (ParseResult(OkP, FailP)) 
import Language.ECMAScript3.PrettyPrint (prettyPrint)

import System.Environment (getArgs)

parseCore filepath = fmap p $ readFile filepath where 
  p source = case P.parse source 0 of
    PG.FailP err -> Left err
    PG.OkP core  -> Right core

main = do
  [filepath] <- getArgs
  coreE <- parseCore filepath
  putStrLn $ either ("Error parsing Core\n"++) 
                 (show . prettyPrint . genModule . simplify . desugarModule)
                 coreE 
  