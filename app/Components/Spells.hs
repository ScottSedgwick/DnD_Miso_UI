{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs #-}
module Components.Spells 
  ( spellsComponent
  ) where

import           Data.Default         ( Default, def )
import           GHC.Generics         ( Generic )
import           Miso                 ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import           Miso.Fetch           ( Response(body, errorMessage), getText )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import           Miso.Lens            ( Lens, (.=), (^.), lens )
import           Miso.JSON            ( eitherDecode )
import           Miso.String          ( intercalate, isInfixOf, toLower )
import           Miso.UI.Accordion    ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner        ( banner )
import           Common.Pages         ( Page(..) )
-- import           Common.Structure     ( Inline(..), renderStructure, rollTable )
import           Model.SpellsModel    ( Spell(..), title, source, level, school, castingTime, range, components, duration, description, lists )
import           Model.MailboxMessage ( spellsTopic, spellsFilterTopic )

data Action
  = GetSpells
  | SetSpells (Response MisoString)
  | PostSpells
  | PostFilter
  | ErrorHandler (Response MisoString)
  | UpdateFilter MisoString

data Model = Model
  { _spellFilter :: MisoString
  , _spells :: Either MisoString [Spell]
  } deriving (Show, Eq, Generic)

instance Default Model where
  def :: Model
  def = Model 
        { _spellFilter = ""
        , _spells = Right []
        }

spellFilter :: Lens Model MisoString
spellFilter = lens _spellFilter $ \m x -> m { _spellFilter = x}

spells :: Lens Model (Either MisoString [Spell])
spells = lens _spells $ \m x -> m { _spells = x}

updateModel :: Action -> Effect a Model Action
updateModel GetSpells        = getText "./data/spells.json" [] SetSpells ErrorHandler
updateModel (SetSpells r)    = spells .= (eitherDecode (body r)) >> issue PostSpells
updateModel PostSpells       = get >>= \m -> either (const $ pure ()) (io_ . publish spellsTopic) (m ^. spells)
updateModel (ErrorHandler r) = maybe (pure ()) mailParent (errorMessage r)
updateModel (UpdateFilter s) = spellFilter .= (fromMisoString s) >> issue PostFilter
updateModel PostFilter       = get >>= \m -> io_ $ publish spellsFilterTopic (m ^. spellFilter)

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Spells
  , H.div_ [] (filterView m : (map spellsView (filteredSpells m)))
  ]

filterView :: Model -> View Model Action
filterView m =
  H.div_ [ P.class_ "gap-3" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. spellFilter), E.onInput UpdateFilter ]
  ]

filteredSpells :: Model -> [Spell]
filteredSpells m = 
  case (m ^. spells) of
    Left err -> [errBg err]
    Right bs -> Prelude.filter (\b -> (toLower $ m ^. spellFilter) `isInfixOf` (toLower $ b ^. title)) bs

errBg :: MisoString -> Spell 
errBg s = def { _title = s }

spellsView :: Spell -> View Model Action
spellsView s = 
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ] 
    [ accordionHeader_ [] [ H.div_ [ P.class_ "header" ] [ text ( ms $ s ^. title ) ] ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.header_ [ P.class_ "border-b px-4 py-3 flex items-center justify-between" ]
          [ H.h2_ [ P.class_ "text-sm font-medium"] [ text ( ms $ s ^. title ) ]
          ]
        , H.div_ [ P.class_ "p-4" ]
          ( titleView s
          <> statblockView s
          <> descriptionView s
          )
        ]
      ]
    ]
  ]

titleView :: Spell -> [View Model Action]
titleView s = [] -- map (\d -> H.p_ [ P.class_ "description" ] [ text ( ms d ) ]) (b ^. description)

statblockView :: Spell -> [View Model Action]
statblockView s = []
  -- [ H.p_ [] 
  --   [ H.strong_ [] [ text "Source: " ]
  --   , H.a_ [ P.target_ "blank", P.href_ (ms $ b ^. sourceurl) ] [ text (ms $ b ^. source ) ]
  --   , H.hr_ []
  --   ] 
  -- ]

descriptionView :: Spell -> [View Model Action]
descriptionView s = []
  -- case ( b ^. proficiencies ) of
  --   Nothing -> equipmentView (b ^. equipment)
  --   Just ps ->
  --     [ H.p_ [] (
  --       [ H.strong_ [] [ text "Skill Proficiencies: " ], text (intercalate ", " $ ps ^. skills), H.br_ []
  --       , H.strong_ [] [ text "Tool Proficiencies: " ], text (intercalate ", " $ ps ^. tools), H.br_ []
  --       , H.strong_ [] [ text "Languages: " ], text (intercalate ", " $ ps ^. languages)
  --       ] <> equipmentView (b ^. equipment)
  --       )
  --     ]

spellsComponent :: [Spell] -> MisoString -> Component parent Model Action
spellsComponent xs filt = 
  if xs == []
    then
      (vcomp def updateModel viewModel) { mount = Just GetSpells }
    else
      (vcomp (def { _spells = Right xs, _spellFilter = filt }) updateModel viewModel)