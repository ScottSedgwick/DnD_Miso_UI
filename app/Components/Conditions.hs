module Components.Conditions
  ( conditionsComponent
  , conditionsTopic
  , module Components.Conditions.Model
  ) where

import           Miso                  ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, publish, text, vcomp )
import           Miso.Fetch            ( Response(body, errorMessage), getText )
import qualified Miso.Html             as H
import qualified Miso.Html.Event       as E
import qualified Miso.Html.Property    as P
import           Miso.Lens             ( (.=), (^.) )
import           Miso.JSON             ( eitherDecode )
import           Miso.PubSub           ( Topic, topic )
import           Miso.String           ( isInfixOf, toLower )
import           Common.Accordion      ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner         ( banner )
import           Common.Eithers        ( hasData )
import           Common.Pages          ( Page(..) )
import           Common.Structure      ( renderStructure )
import           Components.Conditions.Model

data Action
  = GetConditions
  | SetConditions (Response MisoString)
  | PostConditions
  | ErrorHandler (Response MisoString)
  | ErrorUpdate MisoString
  | UpdateFilter MisoString

conditionsTopic :: Topic ConditionsModel
conditionsTopic = topic "conditions"

updateModel :: Action -> Effect a props ConditionsModel Action
updateModel GetConditions     = getText "./data/conditions.json" [] SetConditions ErrorHandler
updateModel (SetConditions r) = conditions .= (eitherDecode (body r)) >> issue PostConditions
updateModel PostConditions    = get >>= io_ . publish conditionsTopic
updateModel (ErrorHandler r) = maybe (issue $ ErrorUpdate "") (issue . ErrorUpdate) (errorMessage r)
updateModel (ErrorUpdate s)  = mailParent s >> io_ (print $ "Error: " <> s)
updateModel (UpdateFilter s) = filterTitle .= (fromMisoString s) >> issue PostConditions

viewModel :: props -> ConditionsModel -> View ConditionsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col" ]
  [ banner Conditions
  , filterView m
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ] (map conditionView (filteredConditions m))
  ]

filterView :: ConditionsModel -> View ConditionsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredConditions :: ConditionsModel -> [Condition]
filteredConditions m =
  case (m ^. conditions) of
    Left err -> [errCondition err]
    Right ps -> filter (\p -> (toLower $ m ^. filterTitle) `isInfixOf` (toLower $ p ^. title)) ps

errCondition :: MisoString -> Condition
errCondition s = Condition
  { _title = s
  , _description = []
  }

conditionView :: Condition -> View ConditionsModel Action
conditionView p =
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ]
    [ accordionHeader_ [] [ H.div_ [ P.class_ "header" ] [ text ( p ^. title ) ] ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.div_ [ P.class_ "p-4" ] ( descriptionView p )
        ]
      ]
    ]
  ]

descriptionView :: Condition -> [View ConditionsModel Action]
descriptionView p = map renderStructure (p ^. description)

conditionsComponent :: ConditionsModel -> Component parent props ConditionsModel Action
conditionsComponent x = (vcomp x updateModel viewModel) { mount = if ( hasData $ _conditions x ) then Nothing else Just GetConditions }
