{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE MultilineStrings   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
module Components.Main ( app ) where

import           Miso( Component, Effect, MisoString, View, (+>), io_, mount, ms, routerSub, styles, subs, subscribe, text, vcomp )
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet)
import           Miso.FFI.QQ (js)
import qualified Miso.Html.Element as H
import           Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens (Lens, (.=), (^.), lens)
import           Miso.Router ( RoutingError, toURI, uriPath )
import qualified Miso.Property as MP
import           Miso.String ( isSuffixOf )
import           Miso.Subscription.History (getURI, pushURI)
import           Miso.Types ( CSS ( Sheet ) )

import           Common.Metadata
import           Common.Pages ( Page(..), allPages, pageImage )
import           Common.SvgImages
import qualified Components.Backgrounds as CB
import qualified Components.Counter as CC
import qualified Components.Home as CH
import qualified Components.Spells as CS
import           Model.BackgroundModel ( Background )
import           Model.SpellsModel ( Spell )
import           Model.MailboxMessage

-----------------------------------------------------------------------------
data Action
  = SetPage Page
  | SetCounter CC.Model
  | SetBackgrounds [Background]
  | SetBackgroundFilter MisoString
  | DisplayError MisoString
  | NavigateTo Page
  | ToggleDarkMode
  | ToggleSidebar
  | ChangeTheme MisoString
  | Subscribe
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model = Model
  { _page :: Page
  , _cval :: CC.Model
  , _err :: Maybe MisoString
  , _backgrounds :: [Background]
  , _backgroundFilter :: MisoString
  , _spells :: [Spell]
  , _spellFilter :: MisoString
  } deriving (Show, Eq)

cval :: Lens Model CC.Model
cval = lens _cval $ \m x -> m { _cval = x }

page :: Lens Model Page
page = lens _page $ \m x -> m { _page = x }

err :: Lens Model (Maybe MisoString)
err = lens _err $ \m x -> m { _err = x }

backgrounds :: Lens Model [Background]
backgrounds = lens _backgrounds $ \m x -> m { _backgrounds = x }

backgroundFilter :: Lens Model MisoString
backgroundFilter = lens _backgroundFilter $ \m x -> m { _backgroundFilter = x }

spells :: Lens Model [Spell]
spells = lens _spells $ \m x -> m { _spells = x }

spellFilter :: Lens Model MisoString
spellFilter = lens _spellFilter $ \m x -> m { _spellFilter = x }

initModel :: Model
initModel = Model
  { _cval = CC.initModel
  , _page = Home
  , _err  = Nothing
  , _backgrounds = []
  , _backgroundFilter = ""
  , _spells = []
  , _spellFilter = ""
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  SetPage p             -> page .= p
  SetCounter x          -> cval .= x
  SetBackgrounds x      -> backgrounds .= x
  SetBackgroundFilter s -> backgroundFilter .= s
  DisplayError e        -> err .= Just e
  NavigateTo p          -> uriSetter p
  ToggleDarkMode        -> io_ [js| return document.dispatchEvent (new CustomEvent('basecoat:theme')); |]
  ToggleSidebar         -> io_ [js| document.dispatchEvent (new CustomEvent('basecoat:sidebar')); |]
  Subscribe             -> subscribe counterTopic SetCounter DisplayError
                        >> subscribe backgroundsTopic SetBackgrounds DisplayError
                        >> subscribe backgroundFilterTopic SetBackgroundFilter DisplayError
  ChangeTheme theme     -> 
    io_ [js| document.documentElement.classList.forEach(c => {
                   if (c.startsWith('theme-')) {
                     document.documentElement.classList.remove(c);
                   }
                 });
                 return document.documentElement.classList.add('theme-' + ${theme}); |]

uriHandler :: Either RoutingError Page -> Action
uriHandler (Left  e) = DisplayError (ms $ show e)
uriHandler (Right p) = SetPage p

uriSetter :: Page -> Effect parent Model Action
uriSetter p = io_ $ do
  baseUri <- getURI
  print baseUri
  let basePath = uriPath baseUri
  let destPath = if (basePath == "" || isSuffixOf ".html" basePath) then basePath else basePath <> "/"
  print destPath
  let pageUri = toURI p
  print pageUri
  let destUri = pageUri { uriPath = destPath }
  print destUri
  pushURI destUri
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel m = H.body_ []
  [ asideView m
  , H.div_ []
    [ topSection
    , H.main_ []
      [ H.div_ [ P.class_ "body-middle" ]
        [ H.section_ []
          ( case m ^. page of
            Home        -> [ "home"    +> CH.home ]
            Counter     -> [ "counter" +> (CC.counter (m ^. cval))]
            Backgrounds -> [ "books"   +> CB.backgroundsComponent (m ^. backgrounds) (m ^. backgroundFilter)]
            Spells      -> [ "spells"  +> CS.spellsComponent (m ^. spells) (m ^. spellFilter)]
          )
        ]
      ]
    , H.footer_ [ P.class_ "error-footer" ]
      [ H.div_ [ P.class_ "error-footer-text" ] 
        ( maybe [] (\e -> [ text e ]) (m ^. err) )
      ]
    ]
  ]

topSection :: View Model Action
topSection = 
  H.header_ []
  [ H.div_ [ P.class_ "flex h-14 w-full items-center gap-2 px-4" ]
    [ H.button_
      [ P.class_ "btn-sm-icon-ghost mr-auto size-7 -ml-1.5"
      , P.data_ "align" "start"
      , P.data_ "side" "bottom"
      , P.data_ "tooltip" "Toggle sidebar"
      , P.aria_ "label" "Toggle sidebar"
      , P.type_ "button"
      , E.onClick ToggleSidebar
      ]
      [ toggleSidebarImage ]
    , H.select_
      [ P.id_ "theme-select"
      , P.class_ "select h-8 leading-none"
      , E.onChange ChangeTheme
      ]
      [ H.option_ [ P.value_ ""] ["Default"]
      , H.option_ [ P.value_ "claude", P.selected_ True ] ["Claude"]
      , H.option_ [ P.value_ "cosmic"] ["Cosmic"]
      , H.option_ [ P.value_ "tangerine"] ["Tangerine"]
      , H.option_ [ P.value_ "supabase"] ["Supabase"]
      ]
    , H.a_
      [ P.data_ "align" "end"
      , P.data_ "side" "bottom"
      , P.data_ "tooltip" "GitHub repository"
      , P.rel_ "noopener noreferrer"
      , P.target_ "_blank"
      , P.class_ "btn-icon size-8"
      , P.href_ githubRepo
      ]
      [ githubImage ]
    -- , H.a_
    --   [ P.data_ "align" "end"
    --   , P.data_ "side" "bottom"
    --   , P.data_ "tooltip" "Discord server"
    --   , P.rel_ "noopener noreferrer"
    --   , P.target_ "_blank"
    --   , P.class_ "btn-icon size-8"
    --   , P.href_ "https://discord.gg/QVDtfYNSxq"
    --   ]
      -- [ discordImage ]
    , H.button_
      [ P.class_ "btn-icon-outline size-8"
      , P.data_ "side" "bottom"
      , P.data_ "tooltip" "Toggle dark mode"
      , P.aria_ "label" "Toggle dark mode"
      , P.type_ "button"
      , E.onClickCapture ToggleDarkMode
      ]
      [ H.span_
        [ P.class_ "hidden dark:block"]
        [ toggleDarkModeImage1 ]
      , H.span_
        [ P.class_ "block dark:hidden"]
        [ toggleDarkModeImage2 ]
      ]
    ]
  ]

asideView :: Model -> View Model Action
asideView _ = 
  H.aside_
  [ P.aria_ "hidden" "true"
  , MP.boolProp "inert" True
  , P.data_ "side" "left"
  , P.class_ "sidebar"
  , P.id_ "sidebar"
  ]
  [ H.nav_ [ P.aria_ "label" "Sidebar navigation" ]
    [ H.header_ [ P.class_ "w-full" ]
      [ H.a_ [ P.class_ "btn-ghost p-2 h-12 w-full justify-start", P.href_ "?page=Home" ]
        [ H.div_ [ P.class_ "bg-sidebar-primary text-sidebar-primary-foreground flex aspect-square size-8 items-center justify-center rounded-lg" ]
          [ dragonImage ]
        , H.div_ [ P.class_ "grid flex-1 text-left text-sm leading-tight" ]
          [ H.span_ [ P.class_ "truncate font-medium"] [ text appTitle ]
          , H.span_ [ P.class_ "truncate text-xs"] [ text appVersion ]
          ]
        ]
      ]
    , H.section_
      [ P.class_ "scrollbar [&_[data-new-link]::after]:content-['New'] [&_[data-new-link]::after]:ml-auto [&_[data-new-link]::after]:text-xs [&_[data-new-link]::after]:font-medium [&_[data-new-link]::after]:bg-sidebar-primary [&_[data-new-link]::after]:text-sidebar-primary-foreground [&_[data-new-link]::after]:px-2 [&_[data-new-link]::after]:py-0.5 [&_[data-new-link]::after]:rounded-md"
      ]
      [ H.div_ [ P.aria_ "labelledby" "group-label-sidebar-content-1", P.role_ "group" ]
        [ H.h3_ [ P.id_ "group-label-sidebar-content-1" ] [ "Applications" ]
        , H.ul_ [] (map mkSideOption allPages)
        ]
      ]
      , H.footer_ []
        [ H.div_ [ P.class_ "popover ", P.id_ "popover-925347" ]
          [ H.button_
            [ P.data_ "keep-mobile-sidebar-open" ""
            , P.class_ "btn-ghost p-2 h-12 w-full flex items-center justify-start"
            , P.aria_ "controls" "popover-925347-popover"
            , P.aria_ "expanded" "false"
            , P.type_ "button"
            , P.id_ "popover-925347-trigger"
            ]
            [ H.img_ [ P.class_ "rounded-lg shrink-0 size-8", P.src_ "https://github.com/dmjio.png" ]
            , H.div_ [ P.class_ "grid flex-1 text-left text-sm leading-tight" ]
              [ H.span_ [ P.class_ "truncate font-medium" ] [ text authorName ]
              , H.span_ [ P.class_ "truncate text-xs"] [ text githubProfile ]
              ]
            , upDownChevrons
            ]
          , H.div_
            [ P.class_ "w-[271px] md:w-[239px]"
            , P.data_ "side" "top"
            , P.aria_ "hidden" "true"
            , P.data_ "popover" ""
            , P.id_ "popover-925347-popover"
            ]
            [ H.div_ [ P.class_ "grid gap-4" ]
              [ H.header_ [ P.class_ "grid gap-1.5" ]
                [ H.h2_ [ P.class_ "font-semibold" ] [ text (appIcon <> " " <> appTitle) ]
                , H.p_ [ P.class_ "text-muted-foreground text-sm" ]
                  [ " Hi, I'm "
                  , H.a_ [ P.target_ "_blank", P.href_ githubProfile, P.class_ "underline underline-offset-4" ] [ text githubUser ]
                  , ", and I'm using "
                  , H.a_ [ P.target_ "_blank", P.href_ "https://github.com/haskell-miso/miso-ui", P.class_ "underline underline-offset-4" ] [ "miso.ui" ]
                  , " to build "
                  , H.a_ [ P.target_ "_blank", P.href_ githubPages, P.class_ "underline underline-offset-4" ] [ text appTitle ]
                  , ". If you find it useful, please consider sponsoring "
                  , H.a_ [ P.target_ "_blank", P.href_ githubProfile, P.class_ "underline underline-offset-4" ] [ text githubUser ]
                  ]
                ]
              , H.footer_ [ P.class_ "grid gap-2" ]
                [ H.a_ [ P.target_ "_blank", P.class_ "btn-sm", P.href_ githubRepo ] [ "Read the code" ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]

mkSideOption :: Page -> View model Action
mkSideOption p = H.li_ [] [ H.a_ [ E.onClick (NavigateTo p) ] [ pageImage p, H.span_ [] [ text (ms (show p)) ] ] ]

-----------------------------------------------------------------------------
app :: Component parent Model Action
app = (vcomp initModel updateModel viewModel)
    { styles = [ Sheet maincss ]
    , subs = [ routerSub uriHandler ]
    , mount = Just Subscribe
    }
-----------------------------------------------------------------------------
maincss :: StyleSheet
maincss =
  CSS.sheet_
  [ CSS.selector_ ".body-middle"
    [ CSS.display "flex"
    , CSS.flexDirection "row"
    , CSS.width "100%"
    , CSS.height "100%"
    ]
  ]