module Common.Pages where

import qualified Data.Map     as M
import           GHC.Generics ( Generic )
import           Miso         ( MisoString, View, fromMisoString, ms )
import           Miso.Router  ( Router, RoutingError(..), URI(..), route, toURI )
import           Text.Read    ( readMaybe )

import           Common.SvgImages

data Page
  = Home
  | Backgrounds
  | Insults
  | Poisons
  | Spells
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

pageImage :: Page -> View model action
pageImage Home = homeImage
pageImage Backgrounds = backgroundIcon
pageImage Insults = insultIcon
pageImage Poisons = poisonIcon
pageImage Spells = spellIcon

pageDescription :: Page -> Maybe MisoString
pageDescription Home        = Nothing
pageDescription Backgrounds = Just "The Backgrounds page allows you to see and read all the available backgrounds for characters."
pageDescription Insults     = Just "The Insults page randomly generates insults (great for Vicious Mockery)."
pageDescription Poisons     = Just "The Poisons page lists known poisons and their effects."
pageDescription Spells      = Just "The Spells page allows you to see and read all the available spells."
