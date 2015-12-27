{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Prompt (module Prompt) where

import           Control.Exception
import           Control.Monad.Free
import           Control.Monad.Free.TH

class Monad m => PromptM m

data Choice = Choice Int

data Response = Finished
              | Choose Choice

data PromptF next = Ask [Choice] (Response -> next)
  deriving Functor

type Prompt = Free PromptF

makeFree ''PromptF

runIO :: Prompt a -> IO a
runIO (Free (Ask options f)) = assert False undefined
runIO (Pure a) = return a

runPure :: Prompt a -> a
runPure (Free (Ask options f)) = assert False undefined
runPure (Pure a) = a
