module Main where

import Bril
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Data.Scientific
import Data.Maybe
import System.IO
import qualified Data.ByteString.Lazy as BS


type Block  = [Instr]
type Blocks = [Block]

showBlockHelper :: Blocks -> String
showBlockHelper (b:bs) = showInstrHelper b ++ "\n" ++ showBlockHelper bs
showBlockHelper [] = ""

terminators :: [Op]
terminators = [Jmp, Br, Ret]

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
                    else  curBlock:blocks in
    formBlocks' instrs newBlocks [instr]

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let blocks = formBlocks $ instrs $ head $ funcs prog
  putStr $ showBlockHelper blocks
  let e = encode prog
  BS.putStrLn e 





