module Types where

import RIO
import RIO.Process

-- | Command line arguments
newtype Options = Options { optionsVerbose :: Bool }
-- data Options = Options
--   { optionsVerbose :: !Bool
--   }

data Application = Application
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

instance HasLogFunc Application where
  logFuncL = lens appLogFunc \x y -> x { appLogFunc = y }
instance HasProcessContext Application where
  processContextL = lens appProcessContext \x y -> x { appProcessContext = y }
instance MonadFail (RIO Application) where
  fail s = logInfo (displayShow s) >> exitFailure
