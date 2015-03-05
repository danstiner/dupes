
import           Options.Applicative

import qualified Command.Commands    as Commands
import           Logging

data Options = Options
  { optCommand :: Commands.Command }

optionParser :: Parser Options
optionParser = Options <$> Commands.parser

main :: IO ()
main = execParser opts >>= runWithOptions where
  parser = helper <*> optionParser
  desc = fullDesc
         <> progDesc "Utility for dealing with large sets of duplicated files"
  opts = info parser desc

runWithOptions :: Options -> IO ()
runWithOptions options = do
  Logging.register ERROR
  Commands.run (optCommand options)
