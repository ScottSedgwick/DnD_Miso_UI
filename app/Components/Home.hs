{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Home 
  ( home
  ) where

import           Miso
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet)
import           Miso.Html.Element as H
import           Miso.Html.Property as P
-----------------------------------------------------------------------------
data Action = Noop
-----------------------------------------------------------------------------
type Model = Int

initModel :: Model
initModel = 0
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  Noop -> pure ()
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel _ = 
  H.div_ [ P.class_ "home-main" ]
  [ H.h1_ [] [ text "Welcome to Miso!" ]
  , H.p_ [] [ text "This application demonstrates Miso's major features." ]
  , H.ul_ [] 
    [ H.li_ [] 
      [ text "The entire application is run by "
      , H.a_ [ P.href_ "https://www.haskell.org/", P.target_ "_blank" ] [ text "Haskell" ]
      , text " and "
      , H.a_ [ P.href_ "https://haskell-miso.org/", P.target_ "_blank" ] [ text "Miso." ]
      ]
    , H.li_ [] 
      [ text "The menu on the left switches pages based on "
      , H.a_ [ P.href_ "https://www.mintlify.com/dmjio/miso/concepts/routing", P.target_ "_blank" ] [ text "routes." ]
      ]
    , H.li_ [] [ text "The Counter page demonstrates event handlers and data binding." ]
    , H.li_ [] [ text "The Books page demonstrates downloading data using REST." ]
    ]
  , H.p_ [] [ text "Have fun writing awesome apps!" ]
  , H.p_ [] [ "Attributions: " ]
  , H.ul_ []
    [ H.li_ [] (map mkAttribution attributions)
    ]
  ]

mkAttribution :: (MisoString, MisoString, MisoString, MisoString) -> View Model Action
mkAttribution (a,b,c,d) =
  H.li_ []
  [ H.a_ [ P.href_ a, P.target_ "_blank"] [ text b ]
  , text " icon by "
  , H.a_ [ P.href_ c, P.target_ "_blank"] [ text d ]
  ]

attributions :: [(MisoString, MisoString, MisoString, MisoString)]
attributions =
  [ ("https://iconscout.com/icons/spellbook", "Spellbook", "https://iconscout.com/contributors/thebeststarticon", "thebeststarticon")
  , ("https://iconscout.com/icons/swearing", "Swearing", "https://iconscout.com/contributors/surang", "Surangkana Jomjunyong")
  , ("https://iconscout.com/icons/teddy-bear", "Teddy Bear", "https://iconscout.com/contributors/icon-click", "Vector Place")
  ]
-----------------------------------------------------------------------------
home :: Component parent Model Action
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