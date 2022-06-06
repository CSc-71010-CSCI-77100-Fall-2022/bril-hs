{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Data.Text

type Prog = [Func] 

data Type = 
    IntType 
  | BoolType 
  deriving (Show)

data Func = Func 
  { name        ::  Text
  , funcArgs    ::  Maybe [Arg] 
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  } 
  deriving Show
 
type Arg = [(Text,Type)] 

data Instr = Instr
  { op        ::  Op
  , dest      ::  Maybe Text
  , instrArgs ::  Maybe [Text]
  , funcs     ::  Maybe [Text]
  , labels    ::  Maybe [Text] } 
  deriving Show

data Op = 
    Add
  | Mul
  | Sub
  | Div
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | Not
  | And
  | Or
  | Jmp
  | Br
  | Call
  | Ret 
  deriving (Show, Generic)
instance FromJSON Op
instance ToJSON Op

main :: IO ()
main = do
  putStrLn "hello world"
