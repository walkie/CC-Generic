
module Examples where

import Data.Maybe (fromJust)

import Choice
import Merge
import Zipper

start :: Expr String String
start = Let ("v" := leaf "x")
      $ "=" :< [
          ""  :< [leaf "twice", Var "v"],
          "+" :< [Var "v", Var "v"]]

twice :: Expr String String
twice = flip exec start 
      $  addDim ("Param" := ["x","y"]) .> inDim
      .> addDim ("Impl" := ["plus","times"]) .> inDim
      .> inBind .> addChoice "Param" [Nothing, Just (leaf "y")] .> up
      .> inUse .> inSub 1 
        .> addChoice "Impl" [Nothing, Just ("*" :< [leaf "2", Var "v"])]

timesX :: Expr String String
timesX = fromJust $ decide [("Param","x"), ("Impl","times")] twice

timesTwoX :: Expr String String
timesTwoX = flip exec timesX
          $  addDim ("Two" := ["literal","variable"]) .> inDim
          .> inUse .> inSub 1 .> inUse .> inSub 0
          .> addChoice "Two" [Nothing, Just (Let ("two" := leaf "2") (Var "two"))]

twice' :: Expr String String
twice' = mergeLR twice timesTwoX
