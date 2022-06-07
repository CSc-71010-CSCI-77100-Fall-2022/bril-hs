{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Scientific
import Data.ByteString.Lazy as BS

{-- BRIL Program Types --}
newtype Prog = Prog [Func]
  deriving Show

data Type = 
    IntType 
  | BoolType 
  deriving (Show)

data Func = Func 
  { name        ::  Text
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  } 
  deriving Show

data Instr = Instr
  { op        ::  Op
  , dest      ::  Maybe Text
  , instrArgs ::  Maybe [Text]
  , funcs     ::  Maybe [Text]
  , labels    ::  Maybe [Text]
  , value     ::  Maybe InstrVal
  }
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
  | Const
  | Print
  deriving Show

instance FromJSON Prog where
  parseJSON = withObject "Prog" $ \v ->
    Prog <$> v .: "functions"

instance ToJSON Prog where
  toJSON (Prog x) = object [ "functions" .= x ]

instance FromJSON Type where
  parseJSON (String s) = pure $ 
    case s of
      "int"   -> IntType
      "bool"  -> BoolType

instance ToJSON Type where 
  toJSON IntType = "int"
  toJSON BoolType = "bool"

instance FromJSON Func where
  parseJSON = withObject "Func" $ \v ->
    Func  <$> v .:  "name"
          <*> v .:? "type"
          <*> v .:  "instrs"

instance ToJSON Func where
  toJSON (Func name funcType instrs) = object [ "name"    .= name, 
                                                "type"    .= funcType,
                                                "instrs"  .= instrs ]

instance FromJSON Instr where
  parseJSON = withObject "Instr" $ \v -> 
    Instr <$> v .:  "op" 
          <*> v .:? "dest"
          <*> v .:? "args"
          <*> v .:? "funcs"
          <*> v .:? "labels"
          <*> v .:? "value"

data InstrVal = BoolVal Bool | IntVal Integer deriving Show
instance FromJSON InstrVal where
  parseJSON (Number x) = return $ IntVal (coefficient x)
  parseJSON (Bool x)   = return $ BoolVal x

instance ToJSON InstrVal where
  toJSON (BoolVal True) = Bool True
  toJSON (BoolVal False) = Bool False
  toJSON (IntVal n) = Number (fromInteger n)

instance ToJSON Instr where
  toJSON (Instr op dest instrArgs funcs labels value) = 
    object [ "op"     .= op,
             "dest"   .= dest,
             "args"   .= instrArgs,
             "funcs"  .= funcs,
             "labels" .= labels,
             "value"  .= value
            ]

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

instance ToJSON Op where
  toJSON Add = "add"
  toJSON Mul = "mul"
  toJSON Sub = "sub"
  toJSON Div = "div"
  toJSON Eq  = "eq"
  toJSON Lt  = "lt"
  toJSON Gt  = "gt"
  toJSON Le  = "le"
  toJSON Ge  = "ge"
  toJSON Not = "not"
  toJSON And = "and"
  toJSON Or  = "or"
  toJSON Jmp = "jmp"
  toJSON Br  = "br"
  toJSON Call = "call"
  toJSON Ret = "ret"
  toJSON Const = "const"
  toJSON Print = "print"

main :: IO ()
main = do
  s <- BS.readFile "add.json"
  let prog = decode s :: Maybe Prog
  print prog
  let e = encode prog
  BS.writeFile "output.json" e 





