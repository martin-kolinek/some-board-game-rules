{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Lens.Traversal
import Control.Lens.At

check :: MonadError e m => Bool -> e -> m ()
check True _ = return ()
check False e = throwError e

checkMaybe :: MonadError e m => e -> Maybe a -> m a
checkMaybe e Nothing = throwError e
checkMaybe _ (Just x) = return x

checkWriter :: MonadWriter [a] m => Bool -> a -> m ()
checkWriter True _ = return ()
checkWriter False a = tell [a]

ixMaybe :: Ixed m => Maybe (Index m) -> Traversal' m (IxValue m)
ixMaybe (Just x) = ix x
ixMaybe _ = ignored
