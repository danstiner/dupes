module Command.Commands (Command(..), commands, run) where

import qualified Command.Init        as Init
import qualified Command.Update      as Update

import           Options.Applicative

data Command = Init Init.Options
             | Update Update.Options

commands :: Mod CommandFields Command
commands =
  command "init" (fmap Init Init.parserInfo)
  <> command "update" (fmap Update Update.parserInfo)

run :: Command -> IO ()
run (Init opt) = Init.run opt
run (Update opt) = Update.run opt
