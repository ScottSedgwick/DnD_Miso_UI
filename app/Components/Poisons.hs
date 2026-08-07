module Components.Poisons
  ( poisonsComponent
  ) where

import           Data.Default          ( Default, def )
import           GHC.Generics          ( Generic )
import           Miso                  ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import           Miso.Fetch            ( Response(body, errorMessage), getText )
import qualified Miso.Html             as H
import qualified Miso.Html.Event       as E
import qualified Miso.Html.Property    as P
import           Miso.Lens             ( Lens, (.=), (^.), lens )
import           Miso.JSON             ( eitherDecode )
import           Miso.String           ( isInfixOf, toLower )
import           Common.Accordion     ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner         ( banner )
import           Common.Pages          ( Page(..) )
import           Common.Structure      ( renderStructure )
import           Model.PoisonModel     ( Poison(..), description, level, title )
import           Model.MailboxMessage  ( poisonFilterTopic, poisonsTopic )

data Action
  = GetPoisons
  | SetPoisons (Response MisoString)
  | PostPoisons
  | PostFilter
  | ErrorHandler (Response MisoString)
  | ErrorUpdate MisoString
  | UpdateFilter MisoString
  | SetPage String

data Model = Model
  { _filterTitle :: MisoString
  , _poisons :: Either MisoString [Poison]
  , _selecteddata :: Maybe String
  } deriving (Show, Eq, Generic)

instance Default Model where
  def :: Model
  def = Model
        { _filterTitle = ""
        , _poisons = Right []
        , _selecteddata = Nothing
        }

filterTitle :: Lens Model MisoString
filterTitle = lens _filterTitle $ \m x -> m { _filterTitle = x}

poisons :: Lens Model (Either MisoString [Poison])
poisons = lens _poisons $ \m x -> m { _poisons = x}

selecteddata :: Lens Model (Maybe String)
selecteddata = lens _selecteddata $ \m x -> m { _selecteddata = x}

updateModel :: Action -> Effect a props Model Action
updateModel GetPoisons       = getText "./data/poisons.json" [] SetPoisons ErrorHandler
updateModel (SetPoisons r)   = poisons .= (eitherDecode (body r)) >> issue PostPoisons
updateModel PostPoisons      = get >>= \m -> either (issue . ErrorUpdate) (io_ . publish poisonsTopic) (m ^. poisons)
updateModel (ErrorHandler r) = maybe (issue $ ErrorUpdate "") (issue . ErrorUpdate) (errorMessage r)
updateModel (ErrorUpdate s)  = mailParent s
updateModel (UpdateFilter s) = filterTitle .= (fromMisoString s) >> issue PostFilter
updateModel PostFilter       = get >>= \m -> io_ $ publish poisonFilterTopic (m ^. filterTitle)
updateModel (SetPage s)      = selecteddata .= Just s

viewModel :: props -> Model -> View Model Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col" ]
  [ banner Poisons
  , filterView m
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ] (map poisonView (filteredPoisons m))
  ]

filterView :: Model -> View Model Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredPoisons :: Model -> [Poison]
filteredPoisons m =
  case (m ^. poisons) of
    Left err -> [errBg err]
    Right ps -> filter (\p -> (toLower $ m ^. filterTitle) `isInfixOf` (toLower $ p ^. title)) ps

errBg :: MisoString -> Poison
errBg s = Poison
  { _title = s
  , _description = []
  , _source = ""
  , _level = Nothing
  }

poisonView :: Poison -> View Model Action
poisonView p =
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ]
    [ accordionHeader_ [] [ H.div_ [ P.class_ "header" ] [ text ( poisonHeader p ) ] ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.div_ [ P.class_ "p-4" ] ( descriptionView p )
        ]
      ]
    ]
  ]

poisonHeader :: Poison -> MisoString
poisonHeader p =
  case (p ^. level) of
    Nothing -> p ^. title
    Just l  -> ms ("[Tier " <> show l <> "] ") <> p ^. title
descriptionView :: Poison -> [View Model Action]
descriptionView p = map renderStructure (p ^. description)

poisonsComponent :: [Poison] -> MisoString -> Component parent props Model Action
poisonsComponent xs filt =
  if xs == []
    then
      (vcomp def updateModel viewModel) { mount = Just GetPoisons }
    else
      (vcomp (def { _poisons = Right xs, _filterTitle = filt }) updateModel viewModel)
