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
    title_       <- o .:? "title" .!= ""
    source_      <- o .:? "source" .!= []
    level_       <- o .:? "level" .!= 0
    school_      <- o .:? "school" .!= ""
    castingTime_ <- o .:? "castingTime" .!= ""
    range_       <- o .:? "range" .!= ""
    components_  <- o .:? "components" .!= ""
    duration_    <- o .:? "duration" .!= ""
    description_ <- o .:? "description" .!= []
    lists_       <- o .:? "lists" .!= []
    pure $ Spell 
      { _title = title_
      , _source = source_
      , _level = level_
      , _school = school_
      , _castingTime = castingTime_
      , _range = range_
      , _components = components_
      , _duration = duration_
      , _description = description_
      , _lists = lists_
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

data SpellFilter = SpellFilter
  { _flt_title :: MisoString
  , _flt_level :: Maybe Int
  , _flt_school :: MisoString
  , _flt_list :: MisoString
  } deriving (Show, Eq)

instance Default SpellFilter where
  def :: SpellFilter
  def = SpellFilter
    { _flt_title = ""
    , _flt_level = Nothing
    , _flt_school = ""
    , _flt_list = ""
    }

instance FromJSON SpellFilter where
  parseJSON :: Value -> Parser SpellFilter
  parseJSON = withObject "Spell" $ \o -> do
    title_  <- o .:? "title" .!= ""
    level_  <- o .:? "level"
    school_ <- o .:? "school" .!= ""
    list_   <- o .:? "list" .!= ""
    pure $ SpellFilter 
      { _flt_title = title_
      , _flt_level = level_
      , _flt_school = school_
      , _flt_list = list_
      }

instance ToJSON SpellFilter where
  toJSON s = 
    object 
    [ "title" .= (_flt_title s)
    , "level" .= (_flt_level s)
    , "school" .= (_flt_school s)
    , "list" .= (_flt_list s)
    ]

flt_title :: Lens SpellFilter  MisoString
flt_title = lens _flt_title $ \m x -> m { _flt_title = x }

flt_level :: Lens SpellFilter  (Maybe Int)
flt_level = lens _flt_level $ \m x -> m { _flt_level = x }

flt_school :: Lens SpellFilter  MisoString
flt_school = lens _flt_school $ \m x -> m { _flt_school = x }

flt_list :: Lens SpellFilter  MisoString
flt_list = lens _flt_list $ \m x -> m { _flt_list = x }
