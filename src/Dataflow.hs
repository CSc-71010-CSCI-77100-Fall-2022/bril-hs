{-# LANGUAGE OverloadedStrings #-}
module Lesson3 where

{--
Artjom Plaunov
Dataflow.hs

Proof of concept for understanding dataflow analysis, before 
building a generic framework.

This runs a reaching definitions analysis on a bril program, 
and returns the analysis results as terminal output (can be used for 
piping into other analysis/transformation tools).
--}

import Bril
import Cfg
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BS
import System.IO
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Definition is a product type of
-- Def (Variable defined * Name of block * Instr # within block).
-- Block numbering starts at 0 for the entry block (denoting arguments).
-- Blocks aside from the entry block are numbered from 1 from the first
-- instruction (including labels as an instruction).
data Def = Def T.Text T.Text Int deriving (Show, Ord)

instance Eq Def where
  Def t1 t2 n1 == Def t3 t4 n2 = t1 == t3 && t2 == t4 && n1 == n2

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let instrs' = instrs $ head $ funcs prog
  let blocks = formBlocks instrs'
  let blockMap = getBlockMap blocks
  let cfg = getCfg blockMap

  -- Reaching Definitions Analysis. (Proof of concept for dataflow).
  
  -- Define in for entry block (arguments).
  let args' = args $ head $ funcs prog
  let vars = fmap (argName <$>) args'
  let entryID = fst $ head blockMap
  let inEntry = case vars of 
        Nothing -> Set.empty
        Just vs -> Set.fromList (map (\x -> Def x entryID 0) vs)

  -- Define inB, a map from blocks to their respective "in" values in terms of
  -- the dataflow analysis.
  -- Initial value for the entry block is the computed init value above.
  let inB' = Map.fromList (map (\x -> (fst x, Set.empty::Set.Set Def)) blockMap)
  let inB = Map.insert entryID inEntry inB'

  -- Define outB, a map from blocks to their respective "out" values.
  -- Out values for all the blocks starts off as an empty set of definitions.
  let outB = Map.fromList (map (\x -> (fst x, Set.empty::Set.Set Def)) blockMap)
  putStrLn $ show $ Map.toList inB

  
  let e = encode $ Prog {funcs = 
      [Func { name = "main", 
              funcType = (Nothing), 
              instrs = instrs', 
              args = args$ head $ funcs prog
            }
      ]}
  putStrLn ""




