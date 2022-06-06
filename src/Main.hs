type Prog = [String]

data Type = IntType | BoolType

data Func = Func 
  { name        ::  String
  , funcArgs    ::  Maybe [Arg] 
  , funcType    ::  Maybe Type
  , instrs      ::  [Instr]
  }
 
type Arg = [(String,Type)]

data Instr = Instr
  { op        ::  Op
  , dest      ::  Maybe String
  , instrArgs ::  Maybe [String]
  , funcs     ::  Maybe [String]
  , labels    ::  Maybe [String] }

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


main :: IO ()
main = do
  putStrLn "hello world"
