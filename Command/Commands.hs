module Command.Commands (
    run
) where

import Command.HashObject

run :: String -> [String] -> IO ()
run commandName args = case commandName of
	"hash-object" -> hashObjectCommand args
	name -> badSubCommand name

badSubCommand :: String -> IO ()
badSubCommand name = do
	return ()
