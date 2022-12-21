{-# LANGUAGE OverloadedStrings #-}
module Tcde where

import Bril
import Cfg

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BS
import System.IO
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M 
import Debug.Trace as D

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let instrs' = instrs $ head $ funcs prog
  let instrs'' = removeDeadCode instrs'
  let instrs''' = concat $ removeUnusedDef <$> Cfg.formBlocks instrs''
  --let blocks = formBlocks $ instrs $ head $ funcs prog
  let e = encode $ Prog {funcs = 
      [Func {name = "main", funcType = (Nothing), instrs = instrs''', args = args $ head $ funcs prog}]}
  BS.putStrLn e

-- trivial dead code no. 2:
-- local basic block optimization - check for redefinitions within a basic 
-- block, such that the first definition is not used at all, e.g.:
-- x = 1;
-- print(5)
-- x = x + 1 - this is a USE of x, so this definition is not deleted yet. 
-- x = 4 - this is a REDEFINITION, so the previous line of code 
-- can be deleted. If we iterate the algorithm again, x=1 also gets deleted.
removeUnusedDef :: [Instr] -> [Instr]
removeUnusedDef instrs = 
    let numberedList = zip [1..(length instrs)] instrs in
    let unusedDefs = removeUnusedDefHelper numberedList M.empty [] in
    let fixpoint = snd <$> filter (\x -> not (fst x `elem` unusedDefs)) numberedList in 
    if length fixpoint == length instrs 
    then fixpoint
    else removeUnusedDef fixpoint
    

-- The core logic is here, 
-- which first checks for uses in an instruction, then for definitions.
removeUnusedDefHelper :: [(Int, Instr)] -> M.Map T.Text Int -> [Int] -> [Int]
removeUnusedDefHelper [] last_def unused = unused
removeUnusedDefHelper (i:is) last_def unused = 
  let last_def' = case instrArgs (snd i) of 
                    Nothing -> last_def
                    Just args -> M.filterWithKey (\k _ -> not (k `elem` args)) last_def
  in
    case dest (snd i) of 
      Nothing -> removeUnusedDefHelper is last_def' unused
      Just def -> 
        case M.lookup def last_def of
          Nothing -> removeUnusedDefHelper is (M.insert def (fst i) last_def') unused 
          Just del_idx -> removeUnusedDefHelper is (M.insert def (fst i) last_def') (del_idx:unused)
        

-- trivial dead code no.1: 
-- check for all uses of variables inside a function. 
-- Then iterate over all definitions, and delete instructions
-- that define variables are not used.
removeDeadCode :: [Instr] -> [Instr]
removeDeadCode instrs = 
  let uses = getUses instrs in
  let fixpoint = filter (checkUsed uses) instrs in 
  if length fixpoint == length instrs
  then fixpoint
  else removeDeadCode fixpoint


checkUsed :: S.Set T.Text -> Instr -> Bool
checkUsed set i = 
  case dest i of
    Nothing -> True
    Just d -> if d `elem` set then True else False

getUses :: [Instr] -> S.Set T.Text
getUses instrs = 
  let set = S.empty in 
  getUses' instrs set

-- getUses recursive helper.
getUses' :: [Instr] -> S.Set T.Text -> S.Set T.Text
getUses' [] set = set
getUses' (i:is) set = 
  let set' = foldl (\set arg -> S.insert arg set) set (getArgs i) in
  getUses' is set'

getArgs :: Instr -> [T.Text]
getArgs instr = fromMaybe [] (instrArgs instr)




