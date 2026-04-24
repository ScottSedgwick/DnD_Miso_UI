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
    [ H.li_ [] 
      [ H.a_ [ P.href_ "https://iconscout.com/icons/spellbook", P.target_ "_blank" ] [ text "Spellbook" ]
      , text " icon by "
      , H.a_ [ P.href_ "https://iconscout.com/contributors/thebeststarticon", P.target_ "_blank"] [ text "thebeststarticon" ]
      , text " on "
      , H.a_ [ P.href_ "https://iconscout.com", P.target_ "_blank"] [ text "IconScout" ]
      ]
    , H.li_ [] 
      [ H.a_ [ P.href_ "https://iconscout.com/icons/teddy-bear", P.target_ "_blank" ] [ text "Teddy Bear" ]
      , text " icon by "
      , H.a_ [ P.href_ "https://iconscout.com/contributors/icon-click", P.target_ "_blank"] [ text "Vector Place" ]
      , text " on "
      , H.a_ [ P.href_ "https://iconscout.com", P.target_ "_blank"] [ text "IconScout" ]
      ]
    ]
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