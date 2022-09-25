{-# LANGUAGE OverloadedStrings #-}
module Lesson2 where

import Bril
import Cfg

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BS
import System.IO

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let eitherProg = eitherDecode s :: Either String Prog
  let prog = case eitherProg of
                    Left err -> error err
                    Right p -> p
  let blocks = formBlocks $ instrs $ head $ funcs prog
  let blockMap = getBlockMap blocks
  let cfg = getCfg blockMap
  putStrLn $ show cfg
  putStrLn ""
  putStrLn $ show blockMap
  --let e = encode prog
  --BS.putStrLn e 





