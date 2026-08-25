module Common.Pages where

import           Data.Default ( Default, def )
import qualified Data.Map     as M
import           GHC.Generics ( Generic )
import           Miso         ( MisoString, View, fromMisoString, ms )
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.Router  ( Router, RoutingError(..), URI(..), route, toURI )
import           Text.Read    ( readMaybe )

import           Common.Attribution

data Page
  = Home
  | Backgrounds
  | Feats
  | Insults
  | Poisons
  | Spells
  deriving stock (Show, Eq, Enum, Bounded, Generic, Read)

instance Default Page where
  def = Home

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
pageImage Home = H.img_ [ P.src_ "assets/home-icon.png", P.width_ "25", P.height_ "60"]
pageImage Backgrounds = H.img_ [ P.src_ "assets/backgrounds-icon.png", P.width_ "25", P.height_ "60"]
pageImage Feats = H.img_ [ P.src_ "assets/feat-icon.png", P.width_ "25", P.height_ "60"]
pageImage Insults = H.img_ [ P.src_ "assets/insult-icon.png", P.width_ "25", P.height_ "60"]
pageImage Poisons = H.img_ [ P.src_ "assets/poison-icon.png", P.width_ "25", P.height_ "60"]
pageImage Spells = H.img_ [ P.src_ "assets/spells-icon.png", P.width_ "25", P.height_ "60"]

pageDescription :: Page -> Maybe MisoString
pageDescription Home        = Nothing
pageDescription Backgrounds = Just "The Backgrounds page allows you to see and read all the available backgrounds for characters."
pageDescription Feats       = Just "The Feats page allows you to see all the Feats available for characters."
pageDescription Insults     = Just "The Insults page randomly generates insults (great for Vicious Mockery)."
pageDescription Poisons     = Just "The Poisons page lists known poisons and their effects."
pageDescription Spells      = Just "The Spells page allows you to see and read all the available spells."

pageAttribution :: Page -> Maybe Attribution
pageAttribution Home        = Just $ Attribution { imageTitle = "House tree home Icon"
                                                 , imageUri = "https://icon-icons.com/"
                                                 , authorName = "Vincent Le Moign"
                                                 , authorUri = "https://icon-icons.com/authors/514-vincent-le-moign" }
pageAttribution Backgrounds = Just $ Attribution { imageTitle = "Teddy bear Icon"
                                                 , imageUri = "https://icon-icons.com/"
                                                 , authorName = "Stefania Servidio"
                                                 , authorUri = "https://icon-icons.com/authors/265-stefania-servidio" }
pageAttribution Feats       = Just $ Attribution { imageTitle = "Champion army reward achievement Icon"
                                                 , imageUri = "https://icon-icons.com/"
                                                 , authorName = "Muhamad Taupik"
                                                 , authorUri = "https://icon-icons.com/authors/1279-muhamad-taupik" }
pageAttribution Insults     = Just $ Attribution { imageTitle = "Obscene gesture"
                                                 , imageUri = "https://icons8.com/icons/set/insult"
                                                 , authorName = "Icons 8"
                                                 , authorUri = "https://icons8.com/" }
pageAttribution Poisons     = Just $ Attribution { imageTitle = "Toxic skull danger poison Icon"
                                                 , imageUri = "https://icon-icons.com/"
                                                 , authorName = "Satawat Anukul"
                                                 , authorUri = "https://icon-icons.com/authors/1312-satawat-anukul" }
pageAttribution Spells      = Just $ Attribution { imageTitle = "Spell Book Icon"
                                                 , imageUri = "https://icon-icons.com/"
                                                 , authorName = "Chanut is Industries"
                                                 , authorUri = "https://icon-icons.com/authors/283-chanut-is-industries" }
