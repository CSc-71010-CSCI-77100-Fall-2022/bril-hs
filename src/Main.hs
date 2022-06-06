type Prog = [Func] 

data Type = 
    IntType 
  | BoolType 
  deriving (Show)

data Func = Func 
  { name        ::  String
  , funcArgs    ::  Maybe [Arg] 
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  } 
  deriving Show
 
type Arg = [(String,Type)] 

data Instr = Instr
  { op        ::  Op
  , dest      ::  Maybe String
  , instrArgs ::  Maybe [String]
  , funcs     ::  Maybe [String]
  , labels    ::  Maybe [String] } 
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
  | Ret deriving Show


main :: IO ()
main = do
  putStrLn "hello world"
