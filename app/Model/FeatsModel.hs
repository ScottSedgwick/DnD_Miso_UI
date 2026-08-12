module Model.FeatsModel where

import           Miso               ( MisoString )
import           Miso.Lens          ( Lens, lens )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, (.:), (.:?), (.=), object, parseJSON, toJSON, withObject )
import           Miso.JSON.Types    ( Value )

import           Common.Structure   ( Structure )

data Feat = Feat
  { _name :: MisoString
  , _source :: MisoString
  , _prerequisite :: Maybe MisoString
  , _description :: [Structure]
  } deriving (Show, Eq)

instance FromJSON Feat where
  parseJSON :: Value -> Parser Feat
  parseJSON = withObject "Feat" $ \o -> do
      n <- o .: "name"
      s <- o .: "source"
      p <- o .:? "prerequisite"
      d <- o .: "description"
      pure $ Feat { _name = n, _source = s, _prerequisite = p, _description = d }
instance ToJSON Feat where
  toJSON p =
    object  [ "name" .= (_name p)
            , "source" .= (_source p)
            , "prerequisite" .= (_prerequisite p)
            , "description" .= (_description p)
            ]

name :: Lens Feat MisoString
name = lens _name $ \m x -> m { _name = x }

source :: Lens Feat MisoString
source = lens _source $ \m x -> m { _source = x }

prerequisite :: Lens Feat (Maybe MisoString)
prerequisite = lens _prerequisite $ \m x -> m { _prerequisite = x }

description :: Lens Feat [Structure]
description = lens _description $ \m x -> m { _description = x }
