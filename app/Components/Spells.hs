{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs #-}
module Components.Spells 
  ( spellsComponent
  ) where

import           Data.Default         ( Default, def )
import           GHC.Generics         ( Generic )
import           Miso                 ( Component (mount), Effect, MisoString, View, io_, issue, mailParent, ms, publish, text, vcomp )
import qualified Miso.CSS             as MC
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
import           Common.Structure     ( renderStructure )
import           Model.SpellsModel    ( Spell(..), title, source, level, school, castingTime, range, components, duration, description, lists,
                                        SpellFilter(..), flt_title, flt_school, flt_list, flt_level ) 
import           Model.MailboxMessage ( spellsTopic, spellFilterTopic )

data Action
  = GetSpells
  | SetSpells (Response MisoString)
  | PostSpells (Either MisoString [Spell])
  | PostFilter SpellFilter
  | ErrorHandler (Response MisoString)
  | UpdateFilter SpellFilter

data Model = Model
  { _spellFilter :: SpellFilter
  , _spells :: Either MisoString [Spell]
  } deriving (Show, Eq, Generic)

instance Default Model where
  def :: Model
  def = Model 
        { _spellFilter = def
        , _spells = Right []
        }

spellFilter :: Lens Model SpellFilter
spellFilter = lens _spellFilter $ \m x -> m { _spellFilter = x}

spells :: Lens Model (Either MisoString [Spell])
spells = lens _spells $ \m x -> m { _spells = x}

updateModel :: Action -> Effect a Model Action
updateModel GetSpells        = getText "./data/spells.json" [] SetSpells ErrorHandler
updateModel (SetSpells r)    = let xs = eitherDecode (body r) in spells .= xs >> issue (PostSpells xs)
updateModel (PostSpells xs)  = either (const $ pure ()) (io_ . publish spellsTopic) xs
updateModel (ErrorHandler r) = maybe (pure ()) mailParent (errorMessage r)
updateModel (UpdateFilter s) = spellFilter .= s >> issue (PostFilter s) >> io_ (print s)
updateModel (PostFilter s)   = io_ $ publish spellFilterTopic s

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [ P.class_ "h-screen flex flex-col"] 
  [ banner Spells
  , filterView m 
  , H.div_ [ P.class_ "overflow-y-auto flex-1"] (map spellsView (filteredSpells m))
  ]

filterView :: Model -> View Model Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4", MC.style_ [ MC.width "100%" ] ]
  [ H.table_ [ MC.style_ [ MC.width "100%" ] ]
    [ H.tr_ [MC.style_ [ MC.width "100%" ] ] 
      [ H.td_ [ MC.style_ [ MC.width "25%" ] ] 
        [ H.label_ [ P.class_ "label", P.for_ "spellFilterTitle" ] [ "Spell Name" ]
        , H.input_ [ P.placeholder_ "Title",  P.class_ "input", P.type_ "text", P.id_ "spellFilterTitle", P.value_ ((m ^. spellFilter) ^. flt_title),  E.onInput (\s -> UpdateFilter ((m ^. spellFilter) { _flt_title = s})) ] 
        ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] 
        [ H.label_ [ P.class_ "label", P.for_ "spellFilterSchool" ] [ "Spell School" ]
        , H.select_ [ MC.style_ [ MC.width "100%"], P.placeholder_ "Spell School", P.class_ "select", P.id_ "spellFilterSchool" , E.onInput (\s -> UpdateFilter ((m ^. spellFilter) { _flt_school = s })) ]
          ( (mkOption "All Schools" "") : map (\s -> mkOption s s) allSchools )
        ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] 
        [ H.label_ [ P.class_ "label", P.for_ "spellFilterLevel" ] [ "Spell Level" ]
        , H.select_ [ MC.style_ [ MC.width "100%"], P.placeholder_ "Spell Level", P.class_ "select", P.id_ "spellFilterLevel" , E.onInput (\s -> UpdateFilter ((m ^. spellFilter) { _flt_level = strToLevel s })) ]
          ( (mkOption "All Levels" "") : map (\l -> mkOption (levelToStr (Just l)) (ms $ show l)) allLevels )
        ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] 
        [ H.label_ [ P.class_ "label", P.for_ "spellFilterList" ] [ "Class Spell List" ]
        , H.select_ [ MC.style_ [ MC.width "100%"], P.placeholder_ "Spell List", P.class_ "select", P.id_ "spellFilterList" , E.onInput (\s -> UpdateFilter ((m ^. spellFilter) { _flt_list = s })) ]
          ( (mkOption "All Lists" "") : map (\s -> mkOption s s) allLists )
        ]
      ]
    ]
  ]

mkOption :: MisoString -> MisoString -> View Model Action
mkOption caption value = H.option_ [ P.value_ value ] [ text caption ]

allSchools :: [MisoString]
allSchools = [ "Abjuration", "Conjuration", "Divination", "Enchantment", "Evocation", "Illusion", "Necromancy", "Transmutation" ]

allLevels :: [Int]
allLevels = [0..9]

allLists :: [MisoString]
allLists = ["Artificer", "Bard", "Cleric", "Druid", "Paladin", "Ranger", "Sorceror", "Warlock", "Wizard"]

levelToStr :: Maybe Int -> MisoString
levelToStr (Just 0) = "Cantrip"
levelToStr (Just 1) = "First level"
levelToStr (Just 2) = "Second level"
levelToStr (Just 3) = "Third level"
levelToStr (Just 4) = "Fourth level"
levelToStr (Just 5) = "Fifth level"
levelToStr (Just 6) = "Sixth level"
levelToStr (Just 7) = "Seventh level"
levelToStr (Just 8) = "Eighth level"
levelToStr (Just 9) = "Ninth level"
levelToStr _ = ""

strToLevel :: MisoString -> Maybe Int
strToLevel "Cantrip"       = (Just 0)
strToLevel "First level"   = (Just 1) 
strToLevel "Second level"  = (Just 2) 
strToLevel "Third level"   = (Just 3) 
strToLevel "Fourth level"  = (Just 4) 
strToLevel "Fifth level"   = (Just 5) 
strToLevel "Sixth level"   = (Just 6) 
strToLevel "Seventh level" = (Just 7) 
strToLevel "Eighth level"  = (Just 8) 
strToLevel "Ninth level"   = (Just 9) 
strToLevel "0" = (Just 0)
strToLevel "1" = (Just 1) 
strToLevel "2" = (Just 2) 
strToLevel "3" = (Just 3) 
strToLevel "4" = (Just 4) 
strToLevel "5" = (Just 5) 
strToLevel "6" = (Just 6) 
strToLevel "7" = (Just 7) 
strToLevel "8" = (Just 8) 
strToLevel "9" = (Just 9) 
strToLevel _ = Nothing

filteredSpells :: Model -> [Spell]
filteredSpells m = 
  case (m ^. spells) of
    Left err -> [errBg err]
    Right xs -> Prelude.filter (spellFilterTest (m ^. spellFilter)) xs

spellFilterTest :: SpellFilter -> Spell -> Bool
spellFilterTest f s = titleOk && schoolOk && listOk && levelOk
  where
    titleOk  = (toLower $ f ^. flt_title) `isInfixOf` (toLower $ s ^. title)
    schoolOk = (toLower $ f ^. flt_school) `isInfixOf` (toLower $ s ^. school)
    listOk   = any (\l -> (toLower $ f ^. flt_list) `isInfixOf` (toLower l)) (s ^. lists)
    levelOk  = (Just $ s ^. level) == (f ^. flt_level) || (f ^. flt_level) == Nothing

errBg :: MisoString -> Spell 
errBg s = def { _title = s }

spellsView :: Spell -> View Model Action
spellsView s = 
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ] 
    [ accordionHeader_ [] [ headerView s ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.div_ [ P.class_ "p-4" ]
          ( sourceView s
          <> descriptionView s
          )
        ]
      ]
    ]
  ]

headerView :: Spell -> View Model Action
headerView s =
  H.div_ [ MC.style_ [ MC.width "100%", MC.marginLeft "5px" ] ] 
  [ H.table_ [ MC.style_ [ MC.width "100%" ] ] 
    [ H.tr_ []
      [ H.td_ [ MC.style_ [ MC.width "25%" ] ] [ text ( ms $ s ^. title ) ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] [ text ( ms $ s ^. school ) ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] [ text ( levelToStr $ Just $ s ^. level ) ]
      , H.td_ [ MC.style_ [ MC.width "25%" ] ] [ text ( ms $ intercalate ", " (s ^. lists) ) ]
      ]
    ]
  ]

sourceView :: Spell -> [View Model Action]
sourceView s =
  [ H.table_ [ MC.style_ [ MC.width "100%" ] ]
    [ H.tr_ [ MC.style_ [ MC.width "100%" ] ]
      [ H.td_ [ MC.style_ [ MC.width "20%" ] ] [ H.strong_ [] [ text "Casting Time: " ], text (ms $ s ^. castingTime) ]
      , H.td_ [ MC.style_ [ MC.width "20%" ] ] [ H.strong_ [] [ text "Range: " ], text (ms $ s ^. range) ]
      , H.td_ [ MC.style_ [ MC.width "20%" ] ] [ H.strong_ [] [ text "Components: " ], text (ms $ s ^. components) ]
      , H.td_ [ MC.style_ [ MC.width "20%" ] ] [ H.strong_ [] [ text "Duration: " ], text (ms $ s ^. duration) ]
      , H.td_ [ MC.style_ [ MC.width "20%" ] ] [ H.strong_ [] [ text "Source: " ], text (ms $ intercalate ", " (s ^. source)) ]
      ]

    ] 
  , H.hr_ []
  ]

descriptionView :: Spell -> [View Model Action]
descriptionView s = map renderStructure (s ^. description)

spellsComponent :: [Spell] -> SpellFilter -> Component parent Model Action
spellsComponent xs filt = 
  if xs == []
    then
      (vcomp def updateModel viewModel) { mount = Just GetSpells }
    else
      (vcomp (def { _spells = Right xs, _spellFilter = filt }) updateModel viewModel)