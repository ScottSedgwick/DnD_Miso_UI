module Components.Backgrounds
  ( backgroundsComponent
  , module Components.Backgrounds.Model
  ) where

import           Data.Default          ( def )
import           Miso                  ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import           Miso.Fetch            ( Response(body, errorMessage), getText )
import qualified Miso.Html             as H
import qualified Miso.Html.Event       as E
import qualified Miso.Html.Property    as P
import           Miso.Lens             ( (.=), (^.) )
import           Miso.JSON             ( eitherDecode )
import           Miso.String           ( intercalate, isInfixOf, toLower )
import           Common.Accordion      ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner         ( banner )
import           Common.Pages          ( Page(..) )
import           Common.Structure      ( Inline(..), renderStructure, rollTable )
import           Components.Backgrounds.Model ( BackgroundsModel(..), filterTitle, backgrounds, backgroundsModelTopic,
                                                Background(..), title, description, source, sourceurl, proficiencies, equipment, features, suggested, traits,
                                                BackgroundTraits(..), personality, ideals, bonds, flaws,
                                                BackgroundFeature(..), featureDescription, featureTitle,
                                                BackgroundProficiency(..), languages, skills, tools)

data Action
  = GetBackgrounds
  | SetBackgrounds (Response MisoString)
  | PostBackgrounds
  | ErrorHandler (Response MisoString)
  | ErrorUpdate MisoString
  | UpdateFilter MisoString

updateModel :: Action -> Effect a props BackgroundsModel Action
updateModel GetBackgrounds       = getText "./data/backgrounds.json" [] SetBackgrounds ErrorHandler
updateModel (SetBackgrounds r)   = backgrounds .= (eitherDecode (body r)) >> issue PostBackgrounds
updateModel PostBackgrounds      = get >>= (io_ . publish backgroundsModelTopic)
updateModel (ErrorHandler r)     = maybe (issue $ ErrorUpdate "") (issue . ErrorUpdate) (errorMessage r)
updateModel (ErrorUpdate s)      = mailParent s
updateModel (UpdateFilter s)     = filterTitle .= (fromMisoString s) >> issue PostBackgrounds

viewModel :: props -> BackgroundsModel -> View BackgroundsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col" ]
  [ banner Backgrounds
  , filterView m
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ] (map backgroundView (filteredBackgrounds m))
  ]

filterView :: BackgroundsModel -> View BackgroundsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredBackgrounds :: BackgroundsModel -> [Background]
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

backgroundView :: Background -> View BackgroundsModel Action
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

descriptionView :: Background -> [View BackgroundsModel Action]
descriptionView b = map (\d -> H.p_ [ P.class_ "description" ] [ text ( ms d ) ]) (b ^. description)

sourceView :: Background -> [View BackgroundsModel Action]
sourceView b =
  [ H.p_ []
    [ H.strong_ [] [ text "Source: " ]
    , H.a_ [ P.target_ "blank", P.href_ (ms $ b ^. sourceurl) ] [ text (ms $ b ^. source ) ]
    , H.hr_ []
    ]
  ]

proficienciesView :: Background -> [View BackgroundsModel Action]
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

equipmentView :: [MisoString] -> [ View BackgroundsModel Action ]
equipmentView [] = []
equipmentView xs =
  [ H.br_ []
  , H.strong_ [] [ text "Equipment: " ]
  , text (intercalate ", " xs)
  , H.hr_ []
  ]

featuresView :: [BackgroundFeature] -> [View BackgroundsModel Action]
featuresView [] = []
featuresView xs = ( H.h4_ [ P.class_ "h-4" ] [ text "Features" ] ) : (concatMap featureView xs)

featureView :: BackgroundFeature -> [View BackgroundsModel Action]
featureView f = ( H.h6_ [ P.class_ "h-6" ] [ text (ms $ f ^. featureTitle) ] ) : (map renderStructure (f ^. featureDescription))

suggestedView :: [MisoString] -> [View BackgroundsModel Action]
suggestedView [] = []
suggestedView xs = ( H.h4_ [ P.class_ "h-4" ] [ text "Suggested Characteristics"] ) : map f xs
  where
    f x = H.p_ [] [ text x ]

traitsView :: Maybe BackgroundTraits -> [View BackgroundsModel Action]
traitsView Nothing = []
traitsView (Just t) =
  [ H.div_ [ P.class_ "grid" ]
    [ traitTable "Personality Trait" ( t ^. personality )
    , traitTable "Ideal" ( t ^. ideals )
    , traitTable "Bond" ( t ^. bonds )
    , traitTable "Flaw" ( t ^. flaws )
    ]
  ]

traitTable :: MisoString -> [MisoString] -> View BackgroundsModel Action
traitTable _ [] = H.div_ [] []
traitTable tableName xs =
  H.div_ [ P.class_ "s6" ]
  [ H.h4_ [ P.class_ "h-4" ] [ text $ ms (tableName <> "s") ]
  , rollTable tableName (map (\x -> [T x]) xs)
  ]

backgroundsComponent :: BackgroundsModel -> Component parent props BackgroundsModel Action
backgroundsComponent xs =
  case (xs ^. backgrounds) of
    Right (_:_) -> (vcomp xs updateModel viewModel)
    _ -> (vcomp def updateModel viewModel) { mount = Just GetBackgrounds }
