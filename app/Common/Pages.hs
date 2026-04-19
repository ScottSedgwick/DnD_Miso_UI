{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs #-}
module Common.Pages where
  
import qualified Data.Map     as M
import           GHC.Generics ( Generic )
import           Miso         ( View, fromMisoString, ms )
import           Miso.Router  ( Router, RoutingError(..), URI(..), route, toURI )
import           Text.Read    ( readMaybe )

import           Common.SvgImages

data Page
  = Home
  | Counter
  | Backgrounds
  deriving stock (Show, Eq, Enum, Bounded, Generic, Read)

instance Router Page where
  toURI :: Page -> URI
  toURI p = URI { uriPath = "", uriFragment = "./", uriQueryString = M.fromList [("page", Just (ms $ show p))]}
  route :: URI -> Either RoutingError Page
  route uri = 
    case M.lookup "page" (uriQueryString uri) of
      Nothing -> Left (NoParses (ms (show uri)))
      Just g ->
        case g of
          Nothing -> Left (NoParses (ms (show uri)))
          Just h ->
            case (readMaybe (fromMisoString h) :: Maybe Page) of
              Nothing -> Left (NoParses (ms (show uri)))
              Just p  -> Right p

allPages :: [Page]
allPages = [minBound .. maxBound]

pageImage :: Page -> View model action
pageImage Home = homeImage
pageImage Counter = counterImage
pageImage Backgrounds = backgroundIcon