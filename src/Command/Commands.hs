module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.AddDupe as AddDupe
import qualified Command.HashObject as HashObject
import qualified Command.LsDupes as LsDupes
import qualified Command.LsFiles as LsFiles
import qualified Command.UpdateIndex as UpdateIndex

import Options.Applicative

data Command 
  = AddDupe AddDupe.Options
  | HashObject HashObject.Options
  | UpdateIndex UpdateIndex.Options
  | LsDupes LsDupes.Options
  | LsFiles LsFiles.Options

parser :: Parser Command
parser = subparser (
     ( command "add-dupe" (fmap AddDupe AddDupe.parserInfo) )
  <> ( command "hash-object" (fmap HashObject HashObject.parserInfo) )
  <> ( command "ls-dupes" (fmap LsDupes LsDupes.parserInfo) )
  <> ( command "ls-files" (fmap LsFiles LsFiles.parserInfo) )
  <> ( command "update-index" (fmap UpdateIndex UpdateIndex.parserInfo) )
  )

run :: Command -> IO ()
run (AddDupe opt) = AddDupe.run opt
run (HashObject opt) = HashObject.run opt
run (LsDupes opt) = LsDupes.run opt
run (LsFiles opt) = LsFiles.run opt
run (UpdateIndex opt) = UpdateIndex.run opt
