module Command.Commands (
    Command (..)
  , parser
  , run
) where

import qualified Command.HashObject as HashObject

import Options.Applicative

data Command 
	= HashObject HashObject.Options

parser :: Parser Command
parser = subparser
  ( command "hash-object" (fmap HashObject HashObject.parserInfo) )

run :: Command -> IO ()
run (HashObject opt) = HashObject.run opt
