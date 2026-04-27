{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Model.InsultsModel where

import           Data.Default       ( Default, def )
import           Miso               ( MisoString )
import           Miso.JSON          ( FromJSON, Parser, ToJSON, Value, (.:?), (.!=), (.=), object, parseJSON, toJSON, withObject )
import           Miso.Lens          ( Lens, lens )

data Insults = Insults
  { _insults :: [MisoString]
  } deriving (Show, Eq)

insults :: Lens Insults [MisoString]
insults = lens _insults $ \m x -> m { _insults = x }

instance Default Insults where
  def :: Insults
  def = Insults { _insults = [] }

instance FromJSON Insults where
  parseJSON :: Value -> Parser Insults
  parseJSON = withObject "Insults" $ \o -> do
    i <- o .:? "insults" .!= []
    pure $ Insults { _insults = i }

instance ToJSON Insults where
  toJSON s = object [ "insults" .= (_insults s) ]