module Main where

import Bril
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Data.Scientific
import System.IO
import qualified Data.ByteString.Lazy as BS


terminators :: [Op]
terminators = [Jmp, Br, Ret]

formBlocks :: [Instr] -> [[Instr]]
formBlocks instrs = reverse $ formBlocks' instrs [] [] 

formBlocks' :: [Instr] -> [[Instr]] -> [Instr] -> [[Instr]]
formBlocks' [] blocks block = if    block == [] 
                              then  blocks 
                              else  (reverse block):blocks

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let blocks = formBlocks $ instrs $ head $ funcs prog
  let e = encode prog
  BS.putStr e 





