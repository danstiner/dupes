{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Keep (pureTests) where

import           Pipes
import           Pipes.Parse
import           Pipes.Parse.Ext
import qualified Pipes.Prelude         as P

import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

data Which = LeftOnly
           | RightOnly
           | Both
  deriving Show

instance Arbitrary Which where
  arbitrary = elements [LeftOnly, RightOnly, Both]

keepWhichParser :: (Monad m) => (a -> a -> Which) -> Parser a m (Maybe a)
keepWhichParser which = do
  drawA <- draw
  case drawA of
    Nothing -> return Nothing
    Just a -> do
      drawB <- draw
      case drawB of
        Nothing -> return (Just a)
        Just b ->
          case which a b of
            LeftOnly  -> return (Just a)
            RightOnly -> return (Just b)
            Both      -> unDraw b *> return (Just a)

filterKeeping :: (Monad m) => (a -> a -> Which) -> Producer a m () -> Producer a m ()
filterKeeping which = parsedForever_ (keepWhichParser which)

filterListKeeping :: (a -> a -> Which) -> [a] -> [a]
filterListKeeping which xs = P.toList (filterKeeping which (each xs))

prop_filter_keeping_both_is_identity :: [Int] -> Bool
prop_filter_keeping_both_is_identity xs = filterListKeeping (const . const Both) xs == xs

prop_filter_keeps_at_least_one :: NonEmptyList Int -> Property
prop_filter_keeps_at_least_one (NonEmpty xs) = forAll (filterArbitrarily xs) (not . null)
  where
    filterArbitrarily :: [a] -> Gen [a]
    filterArbitrarily xs = do
      arbitraryWhich <- const . const <$> arbitrary
      return $ filterListKeeping arbitraryWhich xs

prop_filter_terminates :: [Int] -> Which -> Bool
prop_filter_terminates xs which = seq (filterListKeeping (const . const which) xs) True

pureTests = $(testGroupGenerator)
