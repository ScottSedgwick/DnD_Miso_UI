module Components.Feats
  ( featsComponent
  , subtopic
  , module Components.Feats.Model
  ) where

import           Miso                   ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, publish, text, vcomp )
import           Miso.Fetch             ( Response(body, errorMessage), getText )
import qualified Miso.Html              as H
import qualified Miso.Html.Event        as E
import qualified Miso.Html.Property     as P
import           Miso.Lens              ( Lens, (.=), (^.), lens )
import           Miso.JSON              ( eitherDecode )
import           Miso.String            ( isInfixOf, toLower )
import           Common.Accordion       ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner          ( banner )
import           Common.Eithers         ( hasData )
import           Common.DndComponent    ( subtopic )
import           Common.Pages           ( Page(..) )
import           Common.Structure       ( renderStructure )
import           Components.Feats.Model ( Feat, FeatsModel(..), description, prerequisite, name, source )

data Action
  = GetFeats
  | SetFeats (Response MisoString)
  | PostFeatsModel
  | ErrorHandler (Response MisoString)
  | ErrorUpdate MisoString
  | UpdateFilter MisoString

filterTitle :: Lens FeatsModel MisoString
filterTitle = lens _filter $ \m x -> m { _filter = x}

feats :: Lens FeatsModel (Either MisoString [Feat])
feats = lens _feats $ \m x -> m { _feats = x}

updateModel :: Action -> Effect a props FeatsModel Action
updateModel GetFeats         = getText "./data/feats.json" [] SetFeats ErrorHandler
updateModel (SetFeats r)     = feats .= (eitherDecode (body r)) >> issue PostFeatsModel
updateModel PostFeatsModel   = get >>= (io_ . publish subtopic)
updateModel (ErrorHandler r) = (issue . ErrorUpdate) (maybe "" id (errorMessage r))
updateModel (ErrorUpdate s)  = mailParent s >> io_ (print $ "Error: " <> s)
updateModel (UpdateFilter s) = filterTitle .= (fromMisoString s) >> issue PostFeatsModel >> io_ (print s)

viewModel :: props -> FeatsModel -> View FeatsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col" ]
  [ banner Feats
  , filterView m
  , featsOrErrorView (m ^. filterTitle) (m ^. feats)
  ]

featsOrErrorView :: MisoString -> (Either MisoString [Feat]) -> View FeatsModel Action
featsOrErrorView _ (Left err) = H.div_ [ P.class_ "overflow-y-auto flex-1" ] [ text err ]
featsOrErrorView f (Right fs) = H.div_ [ P.class_ "overflow-y-auto flex-1" ] (map featsView (filterFeats f fs))

filterView :: FeatsModel -> View FeatsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filterFeats :: MisoString -> [Feat] -> [Feat]
filterFeats flt fs = filter (\f -> (toLower flt) `isInfixOf` (toLower $ f ^. name)) fs

featsView :: Feat -> View FeatsModel Action
featsView p =
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ]
    [ accordionHeader_ [] [ H.div_ [ P.class_ "header" ] [ text ( featHeader p ) ] ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.div_ [ P.class_ "p-4" ] ( descriptionView p )
        ]
      ]
    ]
  ]

featHeader :: Feat -> MisoString
featHeader p = p ^. name

descriptionView :: Feat -> [View FeatsModel Action]
descriptionView p =
  [ H.h4_ [] [ text (p ^. name) ]
  , H.p_ [] [ H.i_ [] [text ("Source: " <> p ^.  source)] ]
  ]
  <> maybe [] (\pq -> [ H.p_ [] [ H.i_ [] [ text ("Prerequisite: " <> pq)]]]) (p ^. prerequisite)
  <> map renderStructure (p ^. description)

featsComponent :: FeatsModel -> Component parent props FeatsModel Action
featsComponent x = (vcomp x updateModel viewModel) { mount = if ( hasData $ _feats x ) then Nothing else Just GetFeats }
