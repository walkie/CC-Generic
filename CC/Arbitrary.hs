
--
-- A module for generating random, well-formed choice calculus expressions.
--
module CC.Arbitrary where

import Control.Monad (liftM)
import Test.QuickCheck.Gen

import CC.Syntax
import CC.Tree


-----------------------
-- Simple Generators --
-----------------------

-- generate a dimension name
genDimName :: Gen Dim
genDimName = liftM (:[]) (elements ['A'..'Z'])

-- generate a tag name
genTagName :: Gen Tag
genTagName = liftM (:[]) (elements ['a'..'z'])

-- generate a variable name
genVarName :: Gen Var
genVarName = genTagName

-- generate a list of tags
genTagNames :: Gen [Tag]
genTagNames = listOf genTagName


------------------------
-- CC Generator State --
------------------------

data GenState a = GenState {
  maxDepth  :: Int,
  maxBranch :: Int,
  dimEnv    :: [(Dim,Int)],
  varEnv    :: [Var],
  genVal    :: Gen a
}

genState :: Int -> Int -> Gen a -> GenState a
genState d b g = GenState d b [] [] g

inc :: GenState a -> GenState a
inc s = s { maxDepth = maxDepth s - 1 }

addDim :: Dim -> [Tag] -> GenState a -> GenState a
addDim d ts s = s { dimEnv = (d, length ts) : filter ((d/=) . fst) (dimEnv s) }

addVar :: Var -> GenState a -> GenState a
addVar v s = s { varEnv = v : varEnv s }


--------------------------------
-- Choice Calculus Generators --
--------------------------------

type GenCC a = GenState a -> Gen (TreeCC a)

genCC :: TreeVal a => GenCC a
genCC s | maxDepth s < 2 = genLeaf (inc s)
        | otherwise      = oneof (map ($ inc s) gens)
  where gens = [genLeaf, genStr, genLet, genDim] 
            ++ if null (dimEnv s) then [] else [genChc]
            ++ if null (varEnv s) then [] else [genRef]

genLeaf :: GenCC a
genLeaf s = liftM leaf (genVal s)

genStr :: TreeVal a => GenCC a
genStr s = do a  <- genVal s
              es <- resize (maxBranch s) (listOf (genCC s))
              return (node a es)

genLet :: TreeVal a => GenCC a
genLet s = do v <- genVarName
              b <- genCC s
              u <- genCC (addVar v s)
              return (Let v (Bnd b) u)

genRef :: GenCC a
genRef s = liftM Ref (elements (varEnv s))

genDim :: TreeVal a => GenCC a
genDim s = do d  <- genDimName
              ts <- resize (maxBranch s) genTagNames
              e  <- genCC (addDim d ts s)
              return (Dim d ts e)

genChc :: TreeVal a => GenCC a
genChc s = do (d,i) <- elements (dimEnv s)
              as    <- vectorOf i (genCC s)
              return (Chc d as)
