module Main where

import Immediate.Simplify (simplify)
import Immediate.Desugar (desugarModule)
import Immediate.GenerateJs (genModule)
import Language.Core.ParsecParser (parseCore)
import Language.ECMAScript3.PrettyPrint (prettyPrint)

import System.Environment (getArgs)

main = do
  [filepath] <- getArgs
  Right core <- parseCore filepath
  print $ prettyPrint $ genModule $ simplify $ desugarModule core