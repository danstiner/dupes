module Command.Commands (
    Command (..)
  , commands
  , run
) where

import qualified Command.Init        as Init

import           Options.Applicative

data Command
  = Init Init.Options

commands :: Mod CommandFields Command
commands =
     command "init"   (fmap Init   Init.parserInfo)

run :: Command -> IO ()
run (Init opt)   = Init.run opt
