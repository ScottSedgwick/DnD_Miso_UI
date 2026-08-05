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
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  Noop -> pure ()
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ _ =
  H.div_ [ P.class_ "home-main" ]
  [ H.h1_ [] [ text "Welcome to Scott's D&D App!" ]
  , H.ul_ []
    [ H.li_ [] [ text "The Backgrounds page allows you to see and read all the available backgrounds for characters." ]
    , H.li_ [] [ text "The Insults page randomly generates insults (greate for Vicious Mockery)." ]
    , H.li_ [] [ text "The Spells page allows you to see and read all the available spells." ]
    ]
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
    [ H.li_ [] (map mkAttribution attributions)
    ]
  ]

mkAttribution :: (MisoString, MisoString, MisoString, MisoString, MisoString) -> View Model Action
mkAttribution (uri, description, whereused, contributor, name) =
  H.li_ []
  [ H.a_ [ P.href_ uri, P.target_ "_blank"] [ text description ]
  , text (" icon (used in " <> whereused <> ") by ")
  , H.a_ [ P.href_ contributor, P.target_ "_blank"] [ text name ]
  ]

attributions :: [(MisoString, MisoString, MisoString, MisoString, MisoString)]
attributions =
  [ ("https://iconscout.com/icons/spellbook", "Spellbook", "Spells", "https://iconscout.com/contributors/thebeststarticon", "thebeststarticon")
  , ("https://iconscout.com/icons/swearing", "Swearing", "Insults", "https://iconscout.com/contributors/surang", "Surangkana Jomjunyong")
  , ("https://iconscout.com/icons/teddy-bear", "Teddy Bear", "Backgrounds", "https://iconscout.com/contributors/icon-click", "Vector Place")
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
