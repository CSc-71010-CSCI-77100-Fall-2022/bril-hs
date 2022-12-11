{-# LANGUAGE OverloadedStrings #-}
module Bril where
import Data.Aeson
import Data.Text
import Data.Scientific (coefficient)
import System.IO
import Data.ByteString.Lazy as BS

stripNulls xs = Prelude.filter (\(_,v) -> v /= Null) xs

{-- BRIL Program Types --}
newtype Prog = Prog
  { funcs :: [Func] } deriving (Eq, Show)

data Type = 
    IntType 
  | BoolType 
  deriving (Eq, Show)

data Func = Func 
  { name        ::  Text
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  , args        ::  Maybe [Arg]
  } deriving (Eq, Show)

data Arg = Arg
  { argName   ::  Text
  , argType   ::  Type
  }
  deriving (Eq, Show)

data Instr = Instr
  { op        ::  Maybe Op
  , dest      ::  Maybe Text
  , instrArgs ::  Maybe [Text]
  , funcIds   ::  Maybe [Text]
  , labels    ::  Maybe [Text]
  , label     ::  Maybe Text
  , value     ::  Maybe InstrVal
  , instrType      ::  Maybe Type
  }
  deriving (Eq, Show)

data InstrVal = BoolVal Bool | IntVal Integer 
  deriving (Eq, Show)
  
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
  | Id
  deriving (Eq, Show)

{-- BRIL JSON Parsers --}
instance FromJSON Prog where
  parseJSON = withObject "Prog" $ \v ->
    Prog <$> v .: "functions"

instance ToJSON Prog where
  toJSON (Prog x) = object $ stripNulls [ "functions" .= x ]

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
          <*> v .:? "args"

instance ToJSON Func where
  toJSON (Func name funcType instrs args) = object $ stripNulls [ "name"    .= name, 
                                                "type"    .= funcType,
                                                "instrs"  .= instrs,
                                                "args"    .= args]

instance FromJSON Arg where 
  parseJSON = withObject "Arg" $ \v ->
    Arg <$> v .: "name"
        <*> v .: "type"

instance ToJSON Arg where 
  toJSON (Arg argName argType) = 
    object $ [  "name" .= argName,
                "type" .= argType
             ]


instance FromJSON Instr where
  parseJSON = withObject "Instr" $ \v -> 
    Instr <$> v .:? "op" 
          <*> v .:? "dest"
          <*> v .:? "args"
          <*> v .:? "funcs"
          <*> v .:? "labels"
          <*> v .:? "label"
          <*> v .:? "value"
          <*> v .:? "type"

instance ToJSON Instr where
  toJSON (Instr op dest instrArgs funcIds labels label value instrType) = 
    object $ stripNulls [ "op"     .= op,
             "dest"   .= dest,
             "args"   .= instrArgs,
             "funcs"  .= funcIds,
             "labels" .= labels,
             "label"  .= label,
             "value"  .= value,
             "type"   .= instrType
            ]

instance FromJSON InstrVal where
  parseJSON (Number x) = return $ IntVal (coefficient x)
  parseJSON (Bool x)   = return $ BoolVal x

instance ToJSON InstrVal where
  toJSON (BoolVal True) = Bool True
  toJSON (BoolVal False) = Bool False
  toJSON (IntVal n) = Number (fromInteger n)

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
    "id"    -> Id

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
  toJSON Id   = "id"
