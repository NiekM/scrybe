{-# LANGUAGE TemplateHaskell #-}

module Options where

import Import
import RIO.Process

-- | Command line arguments
data Options = Options
  { _optVerbose :: !Bool
  , _optPrelude :: !String
  , _optTimeout :: !Int
  }

makeLenses ''Options

class HasOptions env where
  optionsL :: Lens' env Options

data SynOptions = SynOptions
  { _synPropagate :: Bool
  , _synParametric :: Bool
  , _synPartial :: Bool
  , _synFuel :: Int
  , _synDisjunctions :: Int
  }

makeLenses ''SynOptions

data Command
  = Synth String SynOptions
  | Live String
  | Assert String

class HasCommand env where
  commandL :: Lens' env Command

data Application = Application
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appCommand :: !Command
  }

instance HasLogFunc Application where
  logFuncL = lens appLogFunc \x y -> x { appLogFunc = y }
instance HasProcessContext Application where
  processContextL = lens appProcessContext \x y -> x { appProcessContext = y }
instance MonadFail (RIO Application) where
  fail s = logInfo (displayShow s) >> exitFailure

instance HasOptions Application where
  optionsL = lens appOptions \x y -> x { appOptions = y }
instance HasCommand Application where
  commandL = lens appCommand \x y -> x { appCommand = y }
