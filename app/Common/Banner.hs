{-# LANGUAGE OverloadedStrings #-}
module Common.Banner
  ( banner
  ) where

import           Miso               ( ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P
import           Miso.Types         (View)

import           Common.Pages       ( Page(..) , pageImage )

banner :: Page -> View model action
banner a = H.div_ [ P.class_ "banner" ] 
  [ H.div_ [ P.class_ "banner-item" ] [ pageImage a ]
  , H.div_ [ P.class_ "banner-item"] [ H.h3_ [] [ text $ ms $ show a ] ]
  ]