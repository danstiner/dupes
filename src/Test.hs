module Test (tempNameTemplate) where

import           Language.Haskell.TH.Syntax

tempNameTemplate :: Q Exp
tempNameTemplate = (LitE . StringL) <$> fmap loc_module qLocation
