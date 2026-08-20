module Common.Attribution (Attribution(..)) where

import           Miso

data Attribution = Attribution
  { imageTitle :: MisoString
  , imageUri :: MisoString
  , authorName :: MisoString
  , authorUri :: MisoString
  }
