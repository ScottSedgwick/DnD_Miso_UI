{-# LANGUAGE OverloadedStrings #-}
module Common.Banner
  ( banner
  ) where

import           Miso               ( ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P
import           Miso.Types         (View)

import           Common.Pages ( Page, pageIcon )

banner :: Page -> Miso.Types.View model action
banner a = 
  H.header_ [ P.class_ "primary-container" ]
  [ H.nav_ [] 
    [ H.button_ [ P.class_ "circle transparent" ] [ H.i_ [ P.class_ "responsive" ] [ text (pageIcon a) ] ]
    , H.h6_ [ P.class_ "max center-align" ] [ text (ms $ show a) ]
    , H.button_ [ P.class_ "circle transparent" ] [ H.i_ [ P.class_ "responsive" ] [ text (pageIcon a) ] ]
    ]
  ]