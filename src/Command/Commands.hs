module Command.Commands (
    Command (..)
  , commands
  , run
) where

import qualified Command.Init        as Init
import qualified Command.Keep        as Keep
import qualified Command.Ls          as Ls
import qualified Command.Remove      as Remove
import qualified Command.Update      as Update

import           Options.Applicative

data Command
  = Init Init.Options
  | Keep Keep.Options
  | Ls Ls.Options
  | Remove Remove.Options
  | Update Update.Options

commands :: Mod CommandFields Command
commands =
     command "init"   (fmap Init   Init.parserInfo)
  <> command "keep"   (fmap Keep   Keep.parserInfo)
  <> command "ls"     (fmap Ls     Ls.parserInfo)
  <> command "remove" (fmap Remove Remove.parserInfo)
  <> command "update" (fmap Update Update.parserInfo)

run :: Command -> IO ()
run (Init opt)   = Init.run opt
run (Keep opt)   = Keep.run opt
run (Ls opt)     = Ls.run opt
run (Remove opt) = Remove.run opt
run (Update opt) = Update.run opt
