module Vimscript.Register

import Vimscript.FFI
import Vimscript.List
import Vimscript.Builtins

import Language.Reflection.Elab

%language ElabReflection

%inline
regRdrFmt : Char -> String
regRdrFmt s = 
  case s of 
       ' ' => "default"
       '%' => "current_file_path"
       ':' => "last_command"
       '#' => "alternate_file"
       '/' => "search"
       '=' => "expression"
       '+' => "clipboard"
       '_' => "blackhole"
       '-' => "small_delete"
       '.' => "last_insert"
       s => singleton s

%inline
regRdrName : Char -> TTName
regRdrName s = NS (UN ("prim__readRegister_" ++ regRdrFmt s)) ["Register", "Vimscript"]

mkRegisterReader : Char -> Elab ()
mkRegisterReader r = 
  let 
    reg = regRdrName r
    ty = Declare reg [] `(VIM_IO String)
    fn = DefineFun reg [MkFunClause (Var reg) `(readRegister ~(RConstant (Str (singleton r))))]
  in declareType ty *> defineFunction fn

%runElab (traverse_ mkRegisterReader ['a' .. 'z'])
%runElab (traverse_ mkRegisterReader ['0' .. '9'])

-- why not 
--
-- ```idris 
-- %runElab (traverse_ mkRegisterReader [' ', '%', ':', '#', '/', '=', '+', '_', '-', '.']) 
-- ``` 

-- instead?
-- that is *much* slower, probably because it can't be compiled into an
-- efficient loop of some sort the way this or the previous enumFromTos can

%runElab (traverse_ mkRegisterReader (unpack " %:#/=+_-."))
