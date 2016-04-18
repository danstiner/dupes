import           Commands            (Command)
import qualified Commands
import           Logging

import           Options.Applicative

data Options = Options { optCommand :: Command }

optionParser :: Parser Options
optionParser = Options <$> hsubparser Commands.commands

main :: IO ()
main = execParser opts >>= run
  where
    parser = helper <*> optionParser
    desc = fullDesc
           <> progDesc "Utility for dealing with large sets of duplicated files"
    opts = info parser desc

run :: Options -> IO ()
run options = do
  Logging.register
  Commands.run (optCommand options)
