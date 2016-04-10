{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.Except
import Control.Monad.Writer

check :: MonadError e m => Bool -> e -> m ()
check True _ = return ()
check False e = throwError e

checkMaybe :: MonadError e m => Maybe a -> e -> m a
checkMaybe Nothing e = throwError e
checkMaybe (Just x) _ = return x

checkWriter :: MonadWriter [a] m => Bool -> a -> m ()
checkWriter True _ = return ()
checkWriter False a = tell [a]
