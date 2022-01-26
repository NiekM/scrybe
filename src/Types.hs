module Types where

import RIO
import RIO.Process
import RIO.PrettyPrint
import RIO.PrettyPrint.StylesUpdate

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appStylesUpdate :: !StylesUpdate
  , appUseColor :: !Bool
  , appTermWidth :: !Int
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc \x y -> x { appLogFunc = y }
instance HasProcessContext App where
  processContextL = lens appProcessContext \x y -> x { appProcessContext = y }
instance HasStylesUpdate App where
  stylesUpdateL = lens appStylesUpdate \x y -> x { appStylesUpdate = y }
instance HasTerm App where
  useColorL = lens appUseColor \x y -> x { appUseColor = y }
  termWidthL = lens appTermWidth \x y -> x { appTermWidth = y }
