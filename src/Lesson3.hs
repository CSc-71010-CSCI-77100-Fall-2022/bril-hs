{-# LANGUAGE OverloadedStrings #-}
module Lesson3 where

import Bril
import Cfg

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BS
import System.IO
--import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let instrs' = instrs $ head $ funcs prog
  let instrs'' = removeDeadCode instrs'
  --let blocks = formBlocks $ instrs $ head $ funcs prog
  let e = encode prog
  BS.putStrLn e

-- trivial dead code no.1: 
-- check for all uses of variables inside a function. 
-- Then iterate over all definitions, and delete instructions
-- that define variables are not used.
removeDeadCode :: [Instr] -> [Instr]
removeDeadCode instrs = 
  let uses = getUses instrs in
  filter (checkUsed uses) instrs


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




