module Main where

import Bril
import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Scientific
import System.IO
import Data.ByteString.Lazy as BS

main :: IO ()
main = do
  s <- BS.hGetContents stdin
  let prog = decode s :: Maybe Prog
  print prog
  let e = encode prog
  BS.writeFile "output.json" e 





