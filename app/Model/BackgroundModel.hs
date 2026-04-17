{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Model.BackgroundModel where

import           Miso               ( MisoString )
import           Miso.Lens          ( Lens, lens )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, (.:), (.:?), (.!=), (.=), object, parseJSON, toJSON, withObject )
import           Miso.JSON.Types    ( Value )

import           Common.Structure   ( Structure )

data BackgroundProficiency = BackgroundProficiency
  { _skills :: [MisoString]
  , _tools :: [MisoString]
  , _languages :: [MisoString]
  } deriving (Show, Eq)

instance FromJSON BackgroundProficiency where
  parseJSON :: Value -> Parser BackgroundProficiency
  parseJSON = withObject "BackgroundProficiency" $ \o -> do
    s <- o .:? "skills" .!= []
    t <- o .:? "tools" .!= []
    l <- o .:? "languages" .!= []
    pure $ BackgroundProficiency { _skills = s, _tools = t, _languages = l }
instance ToJSON BackgroundProficiency where
  toJSON b = 
    object [ "skills" .= (_skills b)
           , "tools" .= (_tools b)
           , "toolanguagesls" .= (_languages b)
    ]

skills :: Lens BackgroundProficiency [MisoString]
skills = lens _skills $ \m x -> m { _skills = x }

tools :: Lens BackgroundProficiency [MisoString]
tools = lens _tools $ \m x -> m { _tools = x }

languages :: Lens BackgroundProficiency [MisoString]
languages = lens _languages $ \m x -> m { _languages = x }

data BackgroundFeature = BackgroundFeature
  { _featureTitle :: MisoString
  , _featureDescription :: [Structure]    
  } deriving (Show, Eq)

instance FromJSON BackgroundFeature where
  parseJSON :: Value -> Parser BackgroundFeature
  parseJSON = withObject "BackgroundFeature" $ \o -> do
    t <- o .: "title"
    d <- o .: "description"
    pure $ BackgroundFeature { _featureTitle = t, _featureDescription = d }
instance ToJSON BackgroundFeature where
  toJSON b = 
    object [ "title" .= (_featureTitle b)
           , "description" .= (_featureDescription b)
    ]

featureTitle :: Lens BackgroundFeature MisoString
featureTitle = lens _featureTitle $ \m x -> m { _featureTitle = x }

featureDescription :: Lens BackgroundFeature [Structure]
featureDescription = lens _featureDescription $ \m x -> m { _featureDescription = x }

data BackgroundTraits = BackgroundTraits
  { _personality :: [MisoString]
  , _ideals :: [MisoString]
  , _bonds :: [MisoString]
  , _flaws :: [MisoString]
  } deriving (Show, Eq)

instance FromJSON BackgroundTraits where
  parseJSON :: Value -> Parser BackgroundTraits
  parseJSON = withObject "BackgroundTraits" $ \o -> do
    p <- o.: "personality"
    i <- o .: "ideals"
    b <- o .: "bonds"
    f <- o .: "flaws"
    pure $ BackgroundTraits { _personality = p, _ideals = i, _bonds = b, _flaws = f }
instance ToJSON BackgroundTraits where
  toJSON b = 
    object [ "personality" .= (_personality b)
           , "ideals" .= (_ideals b)
           , "bonds" .= (_bonds b)
           , "flaws" .= (_flaws b)
    ]

personality :: Lens BackgroundTraits [MisoString]
personality = lens _personality $ \m x -> m { _personality = x }

ideals :: Lens BackgroundTraits [MisoString]
ideals = lens _ideals $ \m x -> m { _ideals = x }

bonds :: Lens BackgroundTraits [MisoString]
bonds = lens _bonds $ \m x -> m { _bonds = x }

flaws :: Lens BackgroundTraits [MisoString]
flaws = lens _flaws $ \m x -> m { _flaws = x }

data Background = Background
  { _title :: MisoString
  , _description :: [MisoString]
  , _source :: MisoString
  , _sourceurl :: MisoString
  , _proficiencies :: Maybe BackgroundProficiency
  , _equipment :: [MisoString]
  , _features :: [BackgroundFeature]
  , _suggested :: [MisoString]
  , _traits :: Maybe BackgroundTraits
  } deriving (Show, Eq)
instance FromJSON Background where
  parseJSON :: Value -> Parser Background
  parseJSON = withObject "Background" $ \o -> do
    t <- o .: "title"
    d <- o .: "description"
    s <- o .: "source"
    u <- o .: "sourceurl"
    p <- o .:? "proficiencies"
    e <- o .:? "equipment" .!= []
    f <- o .: "features"
    g <- o .:? "suggested" .!= []
    r <- o .:? "traits"
    pure $ Background { _title = t, _description = d, _source = s, _sourceurl = u, _proficiencies = p, _equipment = e, _features = f, _suggested = g, _traits = r }
instance ToJSON Background where
  toJSON b = 
    object [ "title" .= (_title b)
           , "description" .= (_description b)
           , "source" .= (_source b)
           , "sourceurl" .= (_sourceurl b)
           , "proficiencies" .= (_proficiencies b)
           , "equipment" .= (_equipment b)
           , "features" .= (_features b)
           , "suggested" .= (_suggested b)
           , "traits" .= (_traits b)
           ]

title :: Lens Background MisoString
title = lens _title $ \m x -> m { _title = x }

description :: Lens Background [MisoString]
description = lens _description $ \m x -> m { _description = x }

source :: Lens Background MisoString
source = lens _source $ \m x -> m { _source = x }

sourceurl :: Lens Background MisoString
sourceurl = lens _sourceurl $ \m x -> m { _sourceurl = x }

proficiencies :: Lens Background (Maybe BackgroundProficiency)
proficiencies = lens _proficiencies $ \m x -> m { _proficiencies = x }

equipment :: Lens Background [MisoString]
equipment = lens _equipment $ \m x -> m { _equipment = x }

features :: Lens Background [BackgroundFeature]
features = lens _features $ \m x -> m { _features = x }

suggested :: Lens Background [MisoString]
suggested = lens _suggested $ \m x -> m { _suggested = x }

traits :: Lens Background (Maybe BackgroundTraits)
traits = lens _traits $ \m x -> m { _traits = x }
