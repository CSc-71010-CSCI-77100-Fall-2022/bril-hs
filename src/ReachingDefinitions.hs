{-# LANGUAGE OverloadedStrings #-}
module ReachingDefinitions where

{--
Artjom Plaunov
ReachingDefinition.hs

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
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

-- Definition is a product type of
-- Def (Variable defined * Name of block * Instr # within block).
-- Block numbering starts at 0 for the entry block (denoting arguments).
-- Blocks aside from the entry block are numbered from 1 from the first
-- instruction (including labels as an instruction).
data Def = Def T.Text T.Text Int deriving (Show, Ord)

instance Eq Def where
  Def t1 t2 n1 == Def t3 t4 n2 = t1 == t3 && t2 == t4 && n1 == n2

reachingDef :: M.Map T.Text (S.Set Def) ->
               M.Map T.Text (S.Set Def) ->
               Cfg ->
               Cfg ->
               S.Set T.Text ->
               T.Text ->
               [(T.Text, Block)] ->
               (M.Map T.Text (S.Set Def), M.Map T.Text (S.Set Def))
reachingDef inB outB succs preds worklist entryID blockMap =
  if S.null worklist
  then
    (inB, outB)
  else
    let blockID = S.elemAt 0 worklist
        worklist' = S.delete blockID worklist
        inB' = merge inB outB preds blockID entryID
        block = (maybe [] id (M.lookup blockID (M.fromList blockMap)))
        (outB', changed) = transfer inB' outB blockID block in
      if changed 
      then
        let f succ wl = S.insert succ wl
        in
          let worklist'' = foldr f worklist'
                            (maybe [] id (M.lookup blockID succs))
          in
            reachingDef inB' outB' succs preds worklist'' entryID blockMap
      else reachingDef inB' outB' succs preds worklist' entryID blockMap

merge :: M.Map T.Text (S.Set Def) ->
         M.Map T.Text (S.Set Def) ->
         Cfg ->
         T.Text ->
         T.Text -> 
         M.Map T.Text (S.Set Def)
merge inB outB preds block entryID =
  let f pred lst = (maybe S.empty id (M.lookup pred outB)):lst 
      setList = foldr f [] (maybe [] id (M.lookup block preds)) in
  if block == entryID
  then
    M.insert block
    (S.unions $ (maybe S.empty id (M.lookup entryID inB)):setList) inB
  else M.insert block (S.unions setList) inB
  
  
transfer :: M.Map T.Text (S.Set Def) ->
            M.Map T.Text (S.Set Def) ->
            T.Text ->
            Block ->
            (M.Map T.Text (S.Set Def), Bool)
transfer inB outB blockID block = 
  let  inB' = (maybe S.empty id (M.lookup blockID inB)) 
       outB' = localAnalysis inB' block S.empty S.empty blockID 1 in
    if S.null (S.difference outB' (maybe S.empty id (M.lookup blockID outB)))
    then (outB, False)
    else (M.insert blockID outB' outB, True)
      

localAnalysis :: S.Set Def ->
                 Block ->
                 S.Set Def ->
                 S.Set Def ->
                 T.Text ->
                 Int ->
                 S.Set Def
localAnalysis inB [] def kills blockID n =  S.union def (filterKills inB kills)
localAnalysis inB (i:is) def kills blockID n =
  let x = op i in
    if x `elem` [Just Add, Just Mul, Just Sub, Just Div,
                 Just Const, Just Id, Just Eq]
    then 
      let destVar = maybe "" id (dest i) 
          d = Def (destVar) blockID n in
        localAnalysis inB is (S.insert d def) (S.insert d kills) blockID (n+1)
    else
      localAnalysis inB is def kills blockID (n+1)


filterKills :: S.Set Def ->
               S.Set Def ->
               S.Set Def
filterKills inB kills =
  let predicate (Def id1 _ _) (Def id2 _ _) = if id1 == id2 then False else True
      origamiFold killDef inB' = S.filter (predicate killDef) inB' in
    foldr origamiFold inB (S.toList kills)
  
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
        Nothing -> S.empty
        Just vs -> S.fromList (map (\x -> Def x entryID 0) vs)

  -- Define inB, a map from blocks to their respective "in" values in terms of
  -- the dataflow analysis.
  -- Initial value for the entry block is the computed init value above.
  let inB' = M.fromList (map (\x -> (fst x, S.empty::S.Set Def)) blockMap)
  let inB = M.insert entryID inEntry inB'

  -- Define outB, a map from blocks to their respective "out" values.
  -- Out values for all the blocks starts off as an empty set of definitions.
  let outB = M.fromList (map (\x -> (fst x, S.empty::S.Set Def)) blockMap)
  --putStrLn $ show $ Map.toList inB
  --putStrLn $ show $ Map.toList outB

  let worklist = S.fromList $ map (\x -> fst x) blockMap
  let preds' = reverseCfg cfg
  let preds = M.insert entryID [] preds'
  let (inB', outB') = reachingDef inB outB cfg preds worklist entryID blockMap
  
  putStrLn "IN DEFS:"
  mapM (putStrLn . show) (M.toList inB')

  putStrLn "OUT DEFS:"
  mapM (putStrLn . show) (M.toList outB')

  let e = encode $ Prog {funcs = 
      [Func { name = "main", 
              funcType = (Nothing), 
              instrs = instrs', 
              args = args$ head $ funcs prog
            }
      ]}
  putStrLn ""




