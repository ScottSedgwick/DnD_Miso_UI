module Components.Poisons.Model where

import           Miso               ( MisoString )
import           Miso.Lens          ( Lens, lens )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, (.:), (.:?), (.=), object, parseJSON, toJSON, withObject )
import           Miso.JSON.Types    ( Value )

import           Common.Structure   ( Structure )

data Poison = Poison
  { _title :: MisoString
  , _description :: [Structure]
  , _source :: MisoString
  , _level :: Maybe Int
  } deriving (Show, Eq)
instance FromJSON Poison where
  parseJSON :: Value -> Parser Poison
  parseJSON = withObject "Poison" $ \o -> do
      t <- o .: "title"
      d <- o .: "description"
      s <- o .: "source"
      l <- o .:? "level"
      pure $ Poison { _title = t, _description = d, _source = s, _level = l }
instance ToJSON Poison where
  toJSON p =
    object [ "title" .= (_title p)
           , "description" .= (_description p)
           , "source" .= (_source p)
           , "level" .= (_level p)
           ]

title :: Lens Poison MisoString
title = lens _title $ \m x -> m { _title = x }

description :: Lens Poison [Structure]
description = lens _description $ \m x -> m { _description = x }

source :: Lens Poison MisoString
source = lens _source $ \m x -> m { _source = x }

level :: Lens Poison (Maybe Int)
level = lens _level $ \m x -> m { _level = x }
