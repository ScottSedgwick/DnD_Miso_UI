module Components.Conditions.Model where

import           Data.Default       ( Default, def )
import           Miso               ( MisoString )
import           Miso.Lens          ( Lens, lens )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, (.:), (.:?), (.=), object, parseJSON, toJSON, withObject )
import           Miso.JSON.Types    ( Value )

import           Common.Structure   ( Structure )

data Condition = Condition
  { _title :: MisoString
  , _description :: [Structure]
  } deriving (Show, Eq)
instance FromJSON Condition where
  parseJSON :: Value -> Parser Condition
  parseJSON = withObject "Condition" $ \o -> do
      t <- o .: "title"
      d <- o .: "description"
      pure $ Condition { _title = t, _description = d }
instance ToJSON Condition where
  toJSON p =
    object [ "title" .= (_title p)
           , "description" .= (_description p)
           ]

title :: Lens Condition MisoString
title = lens _title $ \m x -> m { _title = x }

description :: Lens Condition [Structure]
description = lens _description $ \m x -> m { _description = x }

data ConditionsModel = ConditionsModel
  { _filterTitle :: MisoString
  , _conditions :: Either MisoString [Condition]
  } deriving (Show, Eq)

filterTitle :: Lens ConditionsModel MisoString
filterTitle = lens _filterTitle $ \m x -> m { _filterTitle = x}

conditions :: Lens ConditionsModel (Either MisoString [Condition])
conditions = lens _conditions $ \m x -> m { _conditions = x}

instance FromJSON ConditionsModel where
  parseJSON =
    withObject "ConditionsModel" $ \o -> do
      ci <- o .: "filterTitle"
      mp <- o .:? "conditions"
      case mp of
        Just x -> pure $ ConditionsModel { _filterTitle = ci, _conditions = Right x }
        Nothing -> do
          be <- o .:? "conditionsError"
          case be of
            Just e -> pure $ ConditionsModel { _filterTitle = ci, _conditions = Left e }
            Nothing -> pure $ ConditionsModel { _filterTitle = ci, _conditions = Right [] }
instance ToJSON ConditionsModel where
  toJSON b =
    case (_conditions b) of
      Right bs -> object [ "filterTitle" .= (_filterTitle b)
                         , "conditions" .= bs
                         ]
      Left e -> object [ "filterTitle" .= (_filterTitle b)
                       , "conditionsError" .= e
                       ]

instance Default ConditionsModel where
  def :: ConditionsModel
  def = ConditionsModel
        { _filterTitle = ""
        , _conditions = Right []
        }
