{-# LANGUAGE ExplicitForAll, MultiParamTypeClasses #-}
module Common.DndComponent where

import           Miso.PubSub            ( Topic )

class DndComponent model where
  subtopic :: Topic model
