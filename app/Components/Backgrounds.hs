{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs #-}
module Components.Backgrounds 
  ( backgroundsComponent
  ) where

import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           Miso               ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import           Miso.Fetch         ( Response(body, errorMessage), getText )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P
import           Miso.Lens          ( Lens, (.=), (^.), lens )
import           Miso.JSON          ( eitherDecode )
import           Miso.String        ( intercalate, isInfixOf, toLower )
import           Miso.UI.Accordion  ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner      ( banner )
import           Common.Pages       ( Page(..) )
import           Common.Structure   ( Inline(..), renderStructure, rollTable )
import           Model.BackgroundModel
import           Model.MailboxMessage

data Action
  = GetBackgrounds
  | SetBackgrounds (Response MisoString)
  | PostBackgrounds
  | PostFilter
  | ErrorHandler (Response MisoString)
  | UpdateFilter MisoString
  | SetPage String

data Model = Model
  { _filterTitle :: MisoString
  , _backgrounds :: Either MisoString [Background]
  , _selecteddata :: Maybe String
  } deriving (Show, Eq, Generic)

instance Default Model where
  def :: Model
  def = Model 
        { _filterTitle = ""
        , _backgrounds = Right []
        , _selecteddata = Nothing
        }

filterTitle :: Lens Model MisoString
filterTitle = lens _filterTitle $ \m x -> m { _filterTitle = x}

backgrounds :: Lens Model (Either MisoString [Background])
backgrounds = lens _backgrounds $ \m x -> m { _backgrounds = x}

selecteddata :: Lens Model (Maybe String)
selecteddata = lens _selecteddata $ \m x -> m { _selecteddata = x}

updateModel :: Action -> Effect a Model Action
updateModel GetBackgrounds       = getText "./data/backgrounds.json" [] SetBackgrounds ErrorHandler
updateModel (SetBackgrounds r)   = backgrounds .= (eitherDecode (body r)) >> issue PostBackgrounds
updateModel PostBackgrounds      = get >>= \m -> either (const $ pure ()) (io_ . publish backgroundsTopic) (m ^. backgrounds)
updateModel (ErrorHandler r)     = maybe (pure ()) mailParent (errorMessage r)
updateModel (UpdateFilter s)     = filterTitle .= (fromMisoString s) >> issue PostFilter
updateModel PostFilter           = get >>= \m -> io_ $ publish backgroundFilterTopic (m ^. filterTitle)
updateModel (SetPage s)          = selecteddata .= Just s

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Backgrounds
  , H.div_ [] (filterView m : (map backgroundView (filteredBackgrounds m)))
  ]

filterView :: Model -> View Model Action
filterView m =
  H.div_ [ P.class_ "gap-3" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredBackgrounds :: Model -> [Background]
filteredBackgrounds m = 
  case (m ^. backgrounds) of
    Left err -> [errBg err]
    Right bs -> filter (\b -> (toLower $ m ^. filterTitle) `isInfixOf` (toLower $ b ^. title)) bs

errBg :: MisoString -> Background 
errBg s = Background
  { _title = s
  , _description = []
  , _source = ""
  , _sourceurl = ""
  , _proficiencies = Nothing
  , _equipment = []
  , _features = []
  , _suggested = []
  , _traits = Nothing
  } 

backgroundView :: Background -> View Model Action
backgroundView b = 
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ] 
    [ accordionHeader_ [] [ H.div_ [ P.class_ "header" ] [ text ( ms $ b ^. title ) ] ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.header_ [ P.class_ "border-b px-4 py-3 flex items-center justify-between" ]
          [ H.h2_ [ P.class_ "text-sm font-medium"] [ text ( ms $ b ^. title ) ]
          ]
        , H.div_ [ P.class_ "p-4" ]
          ( descriptionView b
          <> sourceView b
          <> proficienciesView b
          <> featuresView (b ^. features)
          <> suggestedView (b ^. suggested)
          <> traitsView (b ^. traits)
          )
        ]
      ]
    ]
  ]

descriptionView :: Background -> [View Model Action]
descriptionView b = map (\d -> H.p_ [ P.class_ "description" ] [ text ( ms d ) ]) (b ^. description)

sourceView :: Background -> [View Model Action]
sourceView b = 
  [ H.p_ [] 
    [ H.strong_ [] [ text "Source: " ]
    , H.a_ [ P.target_ "blank", P.href_ (ms $ b ^. sourceurl) ] [ text (ms $ b ^. source ) ]
    , H.hr_ []
    ] 
  ]

proficienciesView :: Background -> [View Model Action]
proficienciesView b = 
  case ( b ^. proficiencies ) of
    Nothing -> equipmentView (b ^. equipment)
    Just ps ->
      [ H.p_ [] (
        [ H.strong_ [] [ text "Skill Proficiencies: " ], text (intercalate ", " $ ps ^. skills), H.br_ []
        , H.strong_ [] [ text "Tool Proficiencies: " ], text (intercalate ", " $ ps ^. tools), H.br_ []
        , H.strong_ [] [ text "Languages: " ], text (intercalate ", " $ ps ^. languages)
        ] <> equipmentView (b ^. equipment)
        )
      ]

equipmentView :: [MisoString] -> [ View Model Action ]
equipmentView [] = []
equipmentView xs = 
  [ H.br_ []
  , H.strong_ [] [ text "Equipment: " ]
  , text (intercalate ", " xs)
  , H.hr_ []
  ]

featuresView :: [BackgroundFeature] -> [View Model Action]
featuresView [] = []
featuresView xs = ( H.h4_ [ P.class_ "h-4" ] [ text "Features" ] ) : (concatMap featureView xs)

featureView :: BackgroundFeature -> [View Model Action]
featureView f = ( H.h6_ [ P.class_ "h-6" ] [ text (ms $ f ^. featureTitle) ] ) : (map renderStructure (f ^. featureDescription))

suggestedView :: [MisoString] -> [View Model Action]
suggestedView [] = []
suggestedView xs = ( H.h4_ [ P.class_ "h-4" ] [ text "Suggested Characteristics"] ) : map f xs
  where
    f x = H.p_ [] [ text x ]   

traitsView :: Maybe BackgroundTraits -> [View Model Action]
traitsView Nothing = []
traitsView (Just t) = 
  [ H.div_ [ P.class_ "grid" ]
    [ traitTable "Personality Trait" ( t ^. personality )
    , traitTable "Ideal" ( t ^. ideals )
    , traitTable "Bond" ( t ^. bonds )
    , traitTable "Flaw" ( t ^. flaws )
    ]
  ]

traitTable :: MisoString -> [MisoString] -> View Model Action
traitTable _ [] = H.div_ [] []
traitTable tableName xs =
  H.div_ [ P.class_ "s6" ]
  [ H.h4_ [ P.class_ "h-4" ] [ text $ ms (tableName <> "s") ]
  , rollTable tableName (map (\x -> [T x]) xs)
  ]

backgroundsComponent :: [Background] -> MisoString -> Component parent Model Action
backgroundsComponent xs filt = 
  if xs == []
    then
      (vcomp def updateModel viewModel) { mount = Just GetBackgrounds }
    else
      (vcomp (def { _backgrounds = Right xs, _filterTitle = filt }) updateModel viewModel)