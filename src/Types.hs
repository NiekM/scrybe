{-# LANGUAGE TemplateHaskell #-}

module Types where

import Import
import RIO.Process

-- | Command line arguments
data Options = Options
  { _optVerbose :: !Bool
  , _optInput :: !String
  , _optPrelude :: !String
  , _optTimeout :: !Int
  }

makeLenses ''Options

class HasOptions env where
  optionsL :: Lens' env Options

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

instance HasOptions Application where
  optionsL = lens appOptions \x y -> x { appOptions = y }
