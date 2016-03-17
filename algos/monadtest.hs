{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.Reader

data CallBackArgs = CallBackArgs {arg1 :: String, arg2 :: String} deriving Show


ully a = return 10


getXs :: ReaderT CallBackArgs IO Double
getXs = do
  args <- ask
  liftIO $ ully args
          


