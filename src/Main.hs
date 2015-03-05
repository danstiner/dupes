import qualified Command.Commands    as Commands
import           Logging

import           Options.Applicative

data Options = Options
  { optCommand :: Commands.Command }

optionParser :: Parser Options
optionParser = Options <$> subparser Commands.commands

main :: IO ()
main = execParser opts >>= run where
  parser = helper <*> optionParser
  desc = fullDesc
         <> progDesc "Utility for dealing with large sets of duplicated files"
  opts = info parser desc

run :: Options -> IO ()
run options = do
  Logging.register ERROR
  Commands.run (optCommand options)
