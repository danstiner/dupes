{-# LANGUAGE TemplateHaskell #-}

module Keep ( pureTests ) where

import Control.Applicative
import Control.Monad
import Pipes
import qualified Pipes.Prelude as P

import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

data Which = LeftOnly | RightOnly | Both

filterKeeping :: (Monad m) => (a -> a -> Which) -> Pipe a a m ()
filterKeeping which = undefined

filterListKeeping :: (a -> a -> Which) -> [a] -> [a]
filterListKeeping which [] = []
filterListKeeping which (a:b:xs) = case which a b of
    LeftOnly  -> filterListKeeping which (a:xs)
    RightOnly -> filterListKeeping which (b:xs)
    Both      -> a : filterListKeeping which (b:xs)
filterListKeeping which [a] = [a]

--P.toList (filterKeeping which <-< each as)

prop_filter_always_keeping_both_is_id :: [Int] -> Bool
prop_filter_always_keeping_both_is_id xs = filterListKeeping (const . const Both) xs == xs

pureTests = $(testGroupGenerator)
