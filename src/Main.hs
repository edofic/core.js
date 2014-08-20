module Main where

import Immediate.Simplify (simplify)
import Immediate.Desugar (desugarModule)
import Immediate.GenerateJs (genModule)
import Language.Core.Parser (parse)
import Language.Core.ParseGlue (ParseResult(OkP, FailP))
import Language.ECMAScript3.PrettyPrint (prettyPrint)

import System.Environment (getArgs)

main = do
  [filepath] <- getArgs
  source <- readFile filepath
  let corePR = parse source 0
  case corePR of
    FailP err -> putStrLn "Error parsing Core" >> putStrLn err
    OkP core  -> print $ prettyPrint $ genModule $ simplify $ desugarModule core
  