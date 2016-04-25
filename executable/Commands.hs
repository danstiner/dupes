module Commands (Command(..), commands, run) where

import qualified Command.Init        as Init
import qualified Command.List        as List
import qualified Command.Update      as Update

import           Options.Applicative

data Command = Init Init.Options
             | List List.Options
             | Update Update.Options

commands :: Mod CommandFields Command
commands =
  command "init" (fmap Init Init.parserInfo)
  <> command "list" (fmap List List.parserInfo)
  <> command "update" (fmap Update Update.parserInfo)

run :: Command -> IO ()
run (Init opt) = Init.run opt
run (List opt) = List.run opt
run (Update opt) = Update.run opt
