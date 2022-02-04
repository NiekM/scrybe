module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = MkApp
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc \x y -> x { appLogFunc = y }
instance HasProcessContext App where
  processContextL = lens appProcessContext \x y -> x { appProcessContext = y }
