module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.Add as Add
import qualified Command.Keep as Keep
import qualified Command.Ls as Ls
import qualified Command.Init as Init

import Options.Applicative

data Command
  = Add Add.Options
  | Init Init.Options
  | Keep Keep.Options
  | Ls Ls.Options

parser :: Parser Command
parser = subparser (
     command "add"  (fmap Add Add.parserInfo)
  <> command "init" (fmap Init Init.parserInfo)
  <> command "keep" (fmap Keep Keep.parserInfo)
  <> command "ls"   (fmap Ls Ls.parserInfo)
  )

run :: Command -> IO ()
run (Add opt)  = Add.run opt
run (Init opt) = Init.run opt
run (Keep opt) = Keep.run opt
run (Ls opt)   = Ls.run opt
