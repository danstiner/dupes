module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.Dupes as Dupes
import qualified Command.HashObject as HashObject
import qualified Command.Init as Init
import qualified Command.LsFiles as LsFiles
import qualified Command.UpdateIndex as UpdateIndex

import Options.Applicative

data Command
  = Dupes Dupes.Options
  | HashObject HashObject.Options
  | Init Init.Options
  | LsFiles LsFiles.Options
  | UpdateIndex UpdateIndex.Options

parser :: Parser Command
parser = subparser (
     ( command "dupe" (fmap Dupes Dupes.parserInfo) )
  <> ( command "dupes" (fmap Dupes Dupes.parserInfo) )
  <> ( command "hash-object" (fmap HashObject HashObject.parserInfo) )
  <> ( command "init" (fmap Init Init.parserInfo) )
  <> ( command "ls-files" (fmap LsFiles LsFiles.parserInfo) )
  <> ( command "update-index" (fmap UpdateIndex UpdateIndex.parserInfo) )
  )

run :: Command -> IO ()
run (Dupes opt) = Dupes.run opt
run (HashObject opt) = HashObject.run opt
run (Init opt) = Init.run opt
run (LsFiles opt) = LsFiles.run opt
run (UpdateIndex opt) = UpdateIndex.run opt
