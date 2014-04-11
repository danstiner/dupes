module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.HashObject as HashObject
import qualified Command.UpdateIndex as UpdateIndex

import Options.Applicative

data Command 
	= HashObject HashObject.Options
	| UpdateIndex UpdateIndex.Options

parser :: Parser Command
parser = subparser (
     ( command "hash-object" (fmap HashObject HashObject.parserInfo) )
  <> ( command "update-index" (fmap UpdateIndex UpdateIndex.parserInfo) )
  )

run :: Command -> IO ()
run (HashObject opt) = HashObject.run opt
