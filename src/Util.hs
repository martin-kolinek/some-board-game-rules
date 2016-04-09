module Util where

import Control.Monad.Except

check :: MonadError e m => Bool -> e -> m ()
check True _ = return ()
check False e = throwError e

checkMaybe :: MonadError e m => Maybe a -> e -> m a
checkMaybe Nothing e = throwError e
checkMaybe (Just x) _ = return x
