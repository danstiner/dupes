module Command.Dupes (
	Options
  , parserInfo
  , run
) where

import qualified Command.Dupes.Add   as AddDupe
import qualified Command.Dupes.Keep  as KeepDupes
import qualified Command.Dupes.Ls    as LsDupes

import           Options.Applicative

data Options
  = Add AddDupe.Options
  | Keep KeepDupes.Options
  | Ls LsDupes.Options

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Deal with duplicate files")

parser :: Parser Options
parser = subparser (
     command "add" (fmap Add AddDupe.parserInfo)
  <> command "keep" (fmap Keep KeepDupes.parserInfo)
  <> command "ls" (fmap Ls LsDupes.parserInfo)
  )

run :: Options -> IO ()
run (Add opt) = AddDupe.run opt
run (Keep opt) = KeepDupes.run opt
run (Ls opt) = LsDupes.run opt
