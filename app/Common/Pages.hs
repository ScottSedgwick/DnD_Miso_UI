{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE InstanceSigs #-}
module Common.Pages where
  
import qualified Data.Map     as M
import           GHC.Generics ( Generic )
import           Miso         ( MisoString, fromMisoString, ms )
import           Miso.Router  ( Router, RoutingError(..), URI(..), route, toURI )
import           Text.Read    ( readMaybe )

data Page
  = Home
  | Counter
  | Backgrounds
  deriving stock (Show, Eq, Enum, Bounded, Generic, Read)

instance Router Page where
  toURI :: Page -> URI
  toURI p = URI { uriPath = "", uriFragment = "", uriQueryString = M.fromList [("page", Just (ms $ show p))]}
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

pageIcon :: Page -> MisoString
pageIcon Home        = "home"
pageIcon Counter     = "counter"
pageIcon Backgrounds = "history"
-- pageIcon Ciphers     = "password"
-- pageIcon Crafting    = "construction"
-- pageIcon DiceRoller  = "casino"
-- pageIcon Feats       = "trophy"
-- pageIcon Insults     = "partner_reports"
-- pageIcon Lineages    = "group"
-- pageIcon MagicItems  = "star"
-- pageIcon PointBuy    = "calculate"
-- pageIcon Spells      = "explosion"