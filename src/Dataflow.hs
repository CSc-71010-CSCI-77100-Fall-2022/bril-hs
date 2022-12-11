{-# LANGUAGE OverloadedStrings #-}
module Lesson3 where

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
  let blocks = formBlocks instrs'
  let blockMap = getBlockMap blocks
  putStrLn $ show $ funcs prog
  let cfg = getCfg blockMap
  let e = encode $ Prog {funcs = 
      [Func { name = "main", 
              funcType = (Nothing), 
              instrs = instrs', 
              args = args$ head $ funcs prog
            }
      ]}
  putStrLn "end"



