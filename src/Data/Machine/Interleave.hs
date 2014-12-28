
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}

module Data.Machine.Interleave
  (
    MaybeM, MaybeMT
  , MY(..)
  , merge
  , addXM, addYM
  , capXM, capYM
  ) where

import           Control.Category
import           Data.Machine.Is
import           Data.Machine.Process
import           Data.Machine.Source
import           Data.Machine.Type
import           Prelude              hiding (id, (.))

-------------------------------------------------------------------------------
-- Interleave streams non-deterministically with look-ahead
-------------------------------------------------------------------------------

-- | The input descriptor for maybe 'Merge' or 'MergeT'
data MY a b c where
  MaybeX :: MY a b (Maybe a)            -- block waiting on the left input
  MaybeY :: MY a b (Maybe b)            -- block waiting on the right input
  JustX :: MY a b a            -- block waiting on the right input
  JustY :: MY a b b            -- block waiting on the right input



-- | A 'Machine' that can read from two input stream in a non-deterministic manner with look-ahead.
type MaybeM a b c = Machine (MY a b) c

-- | A 'Machine' that can read from two input stream in a non-deterministic manner with look-ahead and monadic side-effects.
type MaybeMT m a b c = MachineT m (MY a b) c

merge :: Monad m => ProcessT m a a' -> ProcessT m b b' -> MaybeMT m a' b' c -> MaybeMT m a b c
merge ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Yield o k           -> return $ Yield o (merge ma mb k)
  Stop                -> return Stop
  Await f MaybeX _        -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . merge k mb $ f $ Just a
    Stop                -> runMachineT $ merge stopped mb $ f Nothing --ff
    Await g Refl fg     -> return . Await (\b -> case b of
                                Just a -> merge (g a) mb $ encased v
                                Nothing -> merge (encased u) mb $ encased v
                                ) MaybeX . merge fg mb $ encased v
  Await f MaybeY _        -> runMachineT mb >>= \u -> case u of
    Yield b k           -> runMachineT . merge ma k $ f $ Just b
    Stop                -> runMachineT $ merge ma stopped $ f Nothing -- ff
    Await g Refl fg     -> return . Await (\c -> case c of
                                Just b -> merge ma (g b) $ encased v
                                Nothing -> merge ma (encased u) $ encased v
                                ) MaybeY . merge ma fg $ encased v
  Await f JustX ff        -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . merge k mb $ f a
    Stop                -> runMachineT $ merge stopped mb ff
    Await g Refl fg     -> return . Await (\a -> merge (g a) mb $ encased v) JustX
                                  . merge fg mb $ encased v
  Await f JustY ff        -> runMachineT mb >>= \u -> case u of
    Yield b k           -> runMachineT . merge ma k $ f b
    Stop                -> runMachineT $ merge ma stopped ff
    Await g Refl fg     -> return . Await (\b -> merge ma (g b) $ encased v) JustY
                                  . merge ma fg $ encased v

-- | Precompose a pipe onto the left input of a interleave.
addXM :: Monad m => ProcessT m a b -> MaybeMT m b c d -> MaybeMT m a c d
addXM p = merge p echo
{-# INLINE addXM #-}

-- | Precompose a pipe onto the right input of a tee.
addYM :: Monad m => ProcessT m b c -> MaybeMT m a c d -> MaybeMT m a b d
addYM = merge echo
{-# INLINE addYM #-}

-- | Tie off one input of a tee by connecting it to a known source.
capXM :: Monad m => SourceT m a -> MaybeMT m a b c -> ProcessT m b c
capXM s t = process (capped Right) (addXM s t)
{-# INLINE capXM #-}

-- | Tie off one input of a tee by connecting it to a known source.
capYM :: Monad m => SourceT m b -> MaybeMT m a b c -> ProcessT m a c
capYM s t = process (capped Left) (addYM s t)
{-# INLINE capYM #-}

-- | Natural transformation used by 'capX' and 'capY'
capped :: (a -> Either a a) -> MY a a b -> a -> b
capped _ MaybeX = Just
capped _ MaybeY = Just
capped _ JustX = id
capped _ JustY = id
{-# INLINE capped #-}
