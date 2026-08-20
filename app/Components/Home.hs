module Components.Home
  ( home
  ) where

import           Data.Maybe         ( mapMaybe )
import           Miso
import qualified Miso.CSS           as CSS
import           Miso.CSS           (StyleSheet)
import           Miso.Html.Element  as H
import           Miso.Html.Property as P

import Common.Attribution
import Common.Pages ( Page, allPages, pageAttribution, pageDescription )
-----------------------------------------------------------------------------
data Action = Noop
-----------------------------------------------------------------------------
type Model = Int

initModel :: Model
initModel = 0
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel Noop = pure ()
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ _ =
  H.div_ [ P.class_ "home-main" ]
  [ H.h1_ [] [ text "Welcome to Scott's D&D App!" ]
  , H.ul_ [] ( map mkComponentDescription (mapMaybe pageDescription allPages) )
  , H.br_ []
  , H.br_ []
  , H.hr_ []
  , H.h3_ [] [ "Technology stack:"]
  , H.ul_ []
    [ H.li_ []
      [ text "The entire application is built with "
      , H.a_ [ P.href_ "https://www.haskell.org/", P.target_ "_blank" ] [ text "Haskell" ]
      , text " and "
      , H.a_ [ P.href_ "https://haskell-miso.org/", P.target_ "_blank" ] [ text "Miso." ]
      ]
    , H.li_ []
      [ text "The menu on the left switches pages based on "
      , H.a_ [ P.href_ "https://www.mintlify.com/dmjio/miso/concepts/routing", P.target_ "_blank" ] [ text "routes." ]
      ]
    ]
  , H.hr_ []
  , H.h3_ [] [ "Attributions: " ]
  , H.ul_ []
    [ H.li_ [] (mapMaybe mkAttribution allPages)
    ]
  ]

mkComponentDescription :: MisoString -> View Model Action
mkComponentDescription s = H.li_ [] [ text s ]

mkAttribution :: Page -> Maybe (View Model Action)
mkAttribution p =
  case (pageAttribution p) of
    Nothing -> Nothing
    Just attr -> Just $
      H.li_ []
      [ H.a_ [ P.href_ (imageUri attr), P.target_ "_blank"] [ text (imageTitle attr) ]
      , text (" icon (used in " <> (ms $ show p) <> ") by ")
      , H.a_ [ P.href_ (authorUri attr), P.target_ "_blank"] [ text (authorName attr) ]
      ]

-----------------------------------------------------------------------------
home :: Component parent props Model Action
home = (vcomp initModel updateModel viewModel)
  { styles = [ Sheet homeSheet ]
  }
-----------------------------------------------------------------------------
homeSheet :: StyleSheet
homeSheet =
  CSS.sheet_
  [ CSS.selector_ ".home-main"
    [ CSS.paddingLeft "24px"
    , CSS.paddingRight "24px"
    , CSS.paddingTop "48px"
    , CSS.paddingBottom "48px"
    ]
  , CSS.selector_ "h1"
    [ CSS.marginTop "0px"
    , CSS.marginBottom "24px"
    ]
  , CSS.selector_ "li"
    [ CSS.marginBottom "4px"
    ]
  , CSS.selector_ "p"
    [ CSS.marginBottom "16px"
    ]
  ]
