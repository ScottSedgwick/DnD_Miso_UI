{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Model.SpellsModel where

import           Data.Default       ( Default, def )
import           Miso               ( MisoString )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, Value, (.:?), (.!=), (.=), object, parseJSON, toJSON, withObject )
import           Miso.Lens          ( Lens, lens )

import           Common.Structure   ( Structure )

data Spell = Spell
  { _title :: MisoString
  , _source :: [MisoString]
  , _level :: Int
  , _school :: MisoString
  , _castingTime :: MisoString
  , _range :: MisoString
  , _components :: MisoString
  , _duration :: MisoString
  , _description :: [Structure]
  , _lists :: [MisoString]
  } deriving (Show, Eq)

title :: Lens Spell MisoString
title = lens _title $ \m x -> m { _title = x }

source :: Lens Spell [MisoString]
source = lens _source $ \m x -> m { _source = x }

level :: Lens Spell Int
level = lens _level $ \m x -> m { _level = x }

school :: Lens Spell MisoString
school = lens _school $ \m x -> m { _school = x }

castingTime :: Lens Spell MisoString
castingTime = lens _castingTime $ \m x -> m { _castingTime = x }

range :: Lens Spell MisoString
range = lens _range $ \m x -> m { _range = x }

components :: Lens Spell MisoString
components = lens _components $ \m x -> m { _components = x }

duration :: Lens Spell MisoString
duration = lens _duration $ \m x -> m { _duration = x }

description :: Lens Spell [Structure]
description = lens _description $ \m x -> m { _description = x }

lists :: Lens Spell [MisoString]
lists = lens _lists $ \m x -> m { _lists = x }

instance Default Spell where
  def :: Spell
  def = Spell
    { _title = ""
    , _source = []
    , _level = 0
    , _school = ""
    , _castingTime = ""
    , _range = ""
    , _components = ""
    , _duration = ""
    , _description = []
    , _lists = []
    }

instance FromJSON Spell where
  parseJSON :: Value -> Parser Spell
  parseJSON = withObject "Spell" $ \o -> do
    title       <- o .:? "title" .!= ""
    source      <- o .:? "source" .!= []
    level       <- o .:? "level" .!= 0
    school      <- o .:? "school" .!= ""
    castingTime <- o .:? "castingTime" .!= ""
    range       <- o .:? "range" .!= ""
    components  <- o .:? "components" .!= ""
    duration    <- o .:? "duration" .!= ""
    description <- o .:? "description" .!= []
    lists       <- o .:? "lists" .!= []
    pure $ Spell 
      { _title = title
      , _source = source
      , _level = level
      , _school = school
      , _castingTime = castingTime
      , _range = range
      , _components = components
      , _duration = duration
      , _description = description
      , _lists = lists
      }

instance ToJSON Spell where
  toJSON s = 
    object 
    [ "title" .= (_title s)
    , "source" .= (_source s)
    , "level" .= (_level s)
    , "school" .= (_school s)
    , "castingTime" .= (_castingTime s)
    , "range" .= (_range s)
    , "components" .= (_components s)
    , "duration" .= (_duration s)
    , "description" .= (_description s)
    , "lists" .= (_lists s)
    ]
