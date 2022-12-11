{-# LANGUAGE OverloadedStrings #-}
module Cfg where

import Bril
import Data.Aeson (eitherDecode, encode)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid
import System.IO
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

{-- A basic block is a list of instructions which satisfy one or more of 
    the following conditions:
    - a terminator ends the basic block (inclusive, i.e. the terminator is 
      part of the basic block).
    - a label ends a basic block (exclusive).
    - end of the program ends a basic block. --}
type Block  = [Instr] 
type Blocks = [Block] 

{-- Control flow graph.
    Map from block ID's to a list of block ID's,
    denoting possible control flow during execution. --}
type Cfg = Map T.Text [T.Text]

{-- Basic block terminators.--}
terminators :: [Op]
terminators = [Jmp, Br, Ret]

{-- getCfg 
    Compute a Cfg from a block map. --}
getCfg :: Map T.Text Block -> Cfg
getCfg blockMap = 
  let lst = Map.toList blockMap 
      emptyMap = Map.empty in
  getCfg' lst emptyMap

{-- getCfg helper --}
getCfg' :: [(T.Text, Block)] -> Cfg -> Cfg
getCfg' [] cfg = cfg
getCfg' ((name, block):tl) cfg = 
  let lastInstr = head $ reverse block in
  case op lastInstr  of
    c
      | c `elem` [Just Jmp, Just Br] -> 
          let Just keys = labels lastInstr in
          let locs = Map.findWithDefault [] name cfg in
            getCfg' tl (Map.insert name (locs <> keys) cfg)

      | c == Just Ret ->
          getCfg' tl (Map.insert name [] cfg)
      | otherwise -> 
          case tl of
            [] -> getCfg' tl (Map.insert name [] cfg)
            (succ,_):_ -> let locs = Map.findWithDefault [] name cfg in
                              getCfg' tl (Map.insert name (locs <> [succ]) cfg)

getBlockMap :: Blocks -> Map T.Text Block
getBlockMap blocks = 
  let emptyMap = Map.empty in
  getBlockMap' blocks emptyMap 0

getBlockMap' [] map _ = map
getBlockMap' (block:blocks) map nextId = 
  case label $ head block of
    Nothing ->  let id = T.pack $ show nextId in
                let map' = Map.insert ("block" `mappend` id) block map in 
                getBlockMap' blocks map' (nextId+1) 
    Just id ->  let map' = Map.insert id (tail block) map in
                getBlockMap' blocks map' nextId


formBlocks :: [Instr] -> Blocks
formBlocks instrs = formBlocks' instrs [] [] 

formBlocks' :: [Instr] -> Blocks -> Block -> Blocks
formBlocks' [] blocks block = if    block == [] 
                              then  blocks 
                              else  reverse $ (reverse block):blocks
formBlocks' (instr:instrs) blocks curBlock = 
  let o = op instr in
  -- Check if op value is present.
  if o /= Nothing 
  then
    -- op value is a terminator.
    let o' = fromJust o in
    if elem o' terminators
    then
      let newBlock = reverse $ instr:curBlock in
      formBlocks' instrs (newBlock:blocks) []       
    -- 'Regular' Instruction.
    else
      formBlocks' instrs blocks (instr:curBlock)  
  -- If there is no op, we are dealing with a label.
  else
    let newBlocks = if    curBlock == []
                    then  blocks
                    else  (reverse curBlock):blocks in
    formBlocks' instrs newBlocks [instr]