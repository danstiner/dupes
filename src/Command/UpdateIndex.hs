module Command.UpdateIndex (
    Options
  , parserInfo
  , run
) where

import Options.Applicative

data Options = Options
  { _optAdd :: Bool
  , _optRemove :: Bool
  , _optRefresh :: Bool
  , _optPath :: [FilePath] }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Register file contents in the working tree to the index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "add"
     <> help "If a specified file isn't in the index already then it's added. Default behaviour is to ignore new files." )
   <*> switch
      ( long "remove"
     <> help "If a specified file is in the index but is missing then it's removed. Default behavior is to ignore removed file." )
   <*> switch
      ( long "refresh"
     <> help "Looks at the current index and checks to see if merges or updates are needed by checking stat() information." )
  <*> many
      ( argument str (metavar "PATH"))

run :: Options -> IO ()
run _ = undefined
