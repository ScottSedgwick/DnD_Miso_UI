module Model.FeatsModel where

import           Data.Default       ( Default, def )
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

data FeatsModel = FeatsModel
  { _feats :: Either MisoString [Feat]
  , _filter :: MisoString
  } deriving (Show, Eq)

instance FromJSON FeatsModel where
  parseJSON = withObject "FeatsModel" $ \o -> do
    fs <- o .:? "feats"
    fe <- o .:? "featsError"
    let f = case fs of
              Just x -> Right x
              Nothing -> case fe of
                           Just y -> Left y
                           Nothing -> Left "Unknown error"
    ft <- o .: "filter"
    pure $ FeatsModel { _feats = f, _filter = ft }
instance ToJSON FeatsModel where
  toJSON m = case (_feats m) of
               Right fs -> object [ "feats" .= fs, "filter" .= (_filter m) ]
               Left fe  -> object [ "featsError" .= fe, "filter" .= (_filter m) ]

instance Default FeatsModel where
  def = FeatsModel { _feats = Right [], _filter = "" }
