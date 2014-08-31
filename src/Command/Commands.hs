module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.Dupes as Dupes
import qualified Command.HashObject as HashObject
import qualified Command.LsFiles as LsFiles
import qualified Command.UpdateIndex as UpdateIndex

import Options.Applicative

data Command
  = Dupes Dupes.Options
  | HashObject HashObject.Options
  | UpdateIndex UpdateIndex.Options
  | LsFiles LsFiles.Options

parser :: Parser Command
parser = subparser (
     ( command "dupe" (fmap Dupes Dupes.parserInfo) )
  <> ( command "dupes" (fmap Dupes Dupes.parserInfo) )
  <> ( command "hash-object" (fmap HashObject HashObject.parserInfo) )
  <> ( command "ls-files" (fmap LsFiles LsFiles.parserInfo) )
  <> ( command "update-index" (fmap UpdateIndex UpdateIndex.parserInfo) )
  )

run :: Command -> IO ()
run (Dupes opt) = Dupes.run opt
run (HashObject opt) = HashObject.run opt
run (LsFiles opt) = LsFiles.run opt
run (UpdateIndex opt) = UpdateIndex.run opt
