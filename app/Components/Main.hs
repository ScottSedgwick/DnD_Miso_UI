{-# LANGUAGE MultilineStrings   #-}
module Components.Main ( app ) where

import           Data.Default              ( Default, def )
import           Miso                      ( Component, Effect, MisoString, View, (+>), io_, mount, ms, routerSub, styles, subs, subscribe, text, vcomp )
import qualified Miso.CSS                  as CSS
import           Miso.CSS                  ( StyleSheet )
import           Miso.DSL                  ( eval )
import           Miso.FFI                  ( dispatchEvent, newEvent )
import qualified Miso.Html.Element         as H
import           Miso.Html.Event           as E
import qualified Miso.Html.Property        as P
import           Miso.Lens                 ( Lens, (.=), (^.), lens )
import           Miso.Router               ( RoutingError, toURI, uriPath )
import qualified Miso.Property             as MP
import           Miso.String               ( isSuffixOf )
import           Miso.Subscription.History ( getURI, pushURI )
import           Miso.Types                ( CSS ( Sheet ) )

import           Common.Metadata
import           Common.Pages ( Page(..), allPages, pageImage )
import           Common.SvgImages
import qualified Components.Backgrounds as CB
import qualified Components.Feats as CF
import qualified Components.Home as CH
import qualified Components.Insults as CI
import qualified Components.Poisons as CP
import qualified Components.Spells as CS

-----------------------------------------------------------------------------
data Action
  = SetPage Page
  | SetBackgrounds CB.BackgroundsModel
  | SetFeatsModel CF.FeatsModel
  | SetSpellsModel CS.SpellsModel
  | SetInsults CI.InsultsModel
  | SetPoisons [CP.Poison]
  | SetPoisonsFilter MisoString
  | DisplayError MisoString MisoString

  | NavigateTo Page
  | ToggleDarkMode
  | ToggleSidebar
  | ChangeTheme MisoString
  | Subscribe
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model = Model
  { _page :: Page
  , _err :: Maybe MisoString
  , _backgrounds :: CB.BackgroundsModel
  , _spellsModel :: CS.SpellsModel
  , _insults :: CI.InsultsModel
  , _poisons :: [CP.Poison]
  , _poisonsFilter :: MisoString
  , _currentInsult :: MisoString
  , _featsModel :: CF.FeatsModel
  } deriving (Show, Eq)

instance Default Model where
  def = Model
    { _page = def
    , _err = def
    , _backgrounds = def
    , _spellsModel = def
    , _insults = def
    , _currentInsult = ""
    , _poisons = def
    , _poisonsFilter = ""
    , _featsModel = def
    }

page :: Lens Model Page
page = lens _page $ \m x -> m { _page = x }

err :: Lens Model (Maybe MisoString)
err = lens _err $ \m x -> m { _err = x }

backgroundsModel :: Lens Model CB.BackgroundsModel
backgroundsModel = lens _backgrounds $ \m x -> m { _backgrounds = x }

spellsModel :: Lens Model CS.SpellsModel
spellsModel = lens _spellsModel $ \m x -> m { _spellsModel = x }

insults :: Lens Model CI.InsultsModel
insults = lens _insults $ \m x -> m { _insults = x }

poisons :: Lens Model [CP.Poison]
poisons = lens _poisons $ \m x -> m { _poisons = x }

poisonsFilter :: Lens Model MisoString
poisonsFilter = lens _poisonsFilter $ \m x -> m { _poisonsFilter = x }

featsModel :: Lens Model CF.FeatsModel
featsModel = lens _featsModel $ \m x -> m { _featsModel = x }

-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  SetPage p             -> page .= p
  SetBackgrounds x      -> backgroundsModel .= x
  SetFeatsModel x       -> featsModel .= x
  SetSpellsModel x      -> spellsModel .= x
  SetInsults x          -> insults .= x
  SetPoisons p          -> poisons .= p
  SetPoisonsFilter pf   -> poisonsFilter .= pf
  DisplayError c e      -> err .= Just (c <> ": " <> e)
  NavigateTo p          -> uriSetter p
  ToggleDarkMode        -> io_ $ newEvent ("basecoat:theme" :: MisoString) >>= dispatchEvent
  ToggleSidebar         -> io_ $ newEvent ("basecoat:sidebar" :: MisoString) >>= dispatchEvent
  Subscribe             -> subscribe CB.backgroundsModelTopic SetBackgrounds (DisplayError "backgroundsModelTopic")
                        >> subscribe CF.subtopic SetFeatsModel (DisplayError "featsModelTopic")
                        >> subscribe CS.spellsModelTopic SetSpellsModel (DisplayError "spellsModelTopic")
                        >> subscribe CI.insultsTopic SetInsults (DisplayError "insultsTopic")
                        >> subscribe CP.poisonsTopic SetPoisons (DisplayError "poisonsTopic")
                        >> subscribe CP.poisonFilterTopic SetPoisonsFilter (DisplayError "poisonFilterTopic")
  ChangeTheme theme     -> io_ $ changeTheme theme

changeTheme :: MisoString -> IO ()
changeTheme theme = do
    let code = """
                document.documentElement.classList.forEach(c => {" <>
                if (c.startsWith('theme-')) {" <>
                    document.documentElement.classList.remove(c);" <>
                }" <>
                }); " <>
                document.documentElement.classList.add('theme-""" <> theme <> "');"
    _ <- eval code
    pure ()

uriHandler :: Either RoutingError Page -> Action
uriHandler (Left  e) = DisplayError "uriHandler" (ms $ show e)
uriHandler (Right p) = SetPage p

uriSetter :: Page -> Effect parent props Model Action
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
viewModel :: props -> Model -> View Model Action
viewModel _ m = H.body_ []
  [ asideView m
  , H.div_ []
    [ topSection
    , H.main_ []
      [ H.div_ [ P.class_ "body-middle" ]
        [ H.section_ []
          ( case m ^. page of
            Home        -> [ "home"    +> CH.home ]
            Backgrounds -> [ "books"   +> CB.backgroundsComponent (m ^. backgroundsModel)]
            Feats       -> [ "feats"   +> CF.featsComponent (m ^. featsModel)]
            Insults     -> [ "insults" +> CI.insultsComponent (m ^. insults)]
            Poisons     -> [ "poisons" +> CP.poisonsComponent (m ^. poisons) (m ^. poisonsFilter)]
            Spells      -> [ "spells"  +> CS.spellsComponent (m ^. spellsModel)]
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
          [ H.img_ [ P.class_ "rounded-lg shrink-0 size-8", P.src_ authorImage ]
          , H.div_ [ P.class_ "grid flex-1 text-left text-sm leading-tight" ]
            [ H.span_ [ P.class_ "truncate font-medium" ] [ text authorName ]
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
                , H.a_ [ P.target_ "_blank", P.href_ misoUrl, P.class_ "underline underline-offset-4" ] [ "Miso" ]
                , ", and "
                , H.a_ [ P.target_ "_blank", P.href_ misoUiUrl, P.class_ "underline underline-offset-4" ] [ "Miso UI" ]
                , " to build a "
                , H.a_ [ P.target_ "_blank", P.href_ githubRepo, P.class_ "underline underline-offset-4" ] [ text appTitle ]
                , "."
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]

mkSideOption :: Page -> View model Action
mkSideOption p = H.li_ [ P.class_ "pointer" ] [ H.a_ [ E.onClick (NavigateTo p) ] [ pageImage p, H.span_ [] [ text (ms (show p)) ] ] ]

-----------------------------------------------------------------------------
app :: Component parent props Model Action
app = (vcomp def updateModel viewModel)
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
