{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import Data.Text

newtype Prog = Prog [Func]
  deriving (Show, Generic)
instance FromJSON Prog where
  parseJSON = withObject "Prog" $ \v ->
    Prog <$> v .: "functions"

data Type = 
    IntType 
  | BoolType 
  deriving (Show)
instance FromJSON Type where
  parseJSON (String s) = pure $ 
    case s of
      "int"   -> IntType
      "bool"  -> BoolType

data Func = Func 
  { name        ::  Text
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  } 
  deriving Show
instance FromJSON Func where
  parseJSON = withObject "Func" $ \v ->
    Func  <$> v .:  "name"
          <*> v .:? "type"
          <*> v .:  "instrs"

data Instr = Instr
  { op        ::  Op
  , dest      ::  Maybe Text
  , instrArgs ::  Maybe [Text]
  , funcs     ::  Maybe [Text]
  , labels    ::  Maybe [Text] } 
  deriving Show
instance FromJSON Instr where
  parseJSON = withObject "Instr" $ \v -> 
    Instr <$> v .:  "op" 
          <*> v .:? "dest"
          <*> v .:? "args"
          <*> v .:? "funcs"
          <*> v .:? "labels"
    
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
  | Const
  | Print
  deriving Show
instance FromJSON Op where
  parseJSON (String s) = pure $ case s of
    "add" -> Add
    "mul" -> Mul
    "sub" -> Sub
    "div" -> Div
    "eq"  -> Eq
    "lt"  -> Lt
    "gt"  -> Gt
    "le"  -> Le
    "ge"  -> Ge
    "not" -> Not
    "and" -> And
    "or"  -> Or
    "jmp" -> Jmp
    "br"  -> Br
    "call" -> Call
    "ret" -> Ret
    "const" -> Const
    "print" -> Print

main :: IO ()
main = do
  putStrLn "hello world"
