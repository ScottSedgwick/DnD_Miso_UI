module Components.Poisons
  ( PoisonsModel
  , poisonsComponent
  , poisonsTopic
  , module Components.Poisons.Model
  ) where

import           Data.Default          ( Default, def )
import           GHC.Generics          ( Generic )
import           Miso                  ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import           Miso.Fetch            ( Response(body, errorMessage), getText )
import qualified Miso.Html             as H
import qualified Miso.Html.Event       as E
import qualified Miso.Html.Property    as P
import           Miso.Lens             ( Lens, (.=), (^.), lens )
import           Miso.JSON             ( FromJSON, ToJSON, (.:), (.:?), eitherDecode, object, parseJSON, toJSON, withObject )
import qualified Miso.JSON             as J
import           Miso.PubSub           ( Topic, topic )
import           Miso.String           ( isInfixOf, toLower )
import           Common.Accordion      ( accordion_, accordionSection_, accordionHeader_, accordionBody_)

import           Common.Banner         ( banner )
import           Common.Eithers        ( hasData )
import           Common.Pages          ( Page(..) )
import           Common.Structure      ( renderStructure )
import           Components.Poisons.Model ( Poison(..), description, level, title )

data Action
  = GetPoisons
  | SetPoisons (Response MisoString)
  | PostPoisons
  | ErrorHandler (Response MisoString)
  | ErrorUpdate MisoString
  | UpdateFilter MisoString
  | SetPage String

data PoisonsModel = PoisonsModel
  { _filterTitle :: MisoString
  , _poisons :: Either MisoString [Poison]
  , _selecteddata :: Maybe String
  } deriving (Show, Eq, Generic)
instance FromJSON PoisonsModel where
  parseJSON =
    withObject "InsultsModel" $ \o -> do
      ci <- o .: "filterTitle"
      sd <- o .:? "selectedData"
      mp <- o .:? "poisons"
      case mp of
        Just x -> pure $ PoisonsModel { _filterTitle = ci, _selecteddata = sd, _poisons = Right x }
        Nothing -> do
          be <- o .:? "poisonsError"
          case be of
            Just e -> pure $ PoisonsModel { _filterTitle = ci, _selecteddata = sd, _poisons = Left e }
            Nothing -> pure $ PoisonsModel { _filterTitle = ci, _selecteddata = sd, _poisons = Right [] }
instance ToJSON PoisonsModel where
  toJSON b =
    case (_poisons b) of
      Right bs -> case (_selecteddata b) of
                    Nothing -> object [ "filterTitle" J..= (_filterTitle b)
                                      , "poisons" J..= bs
                                      ]
                    Just sd -> object [ "filterTitle" J..= (_filterTitle b)
                                      , "poisons" J..= bs
                                      , "selectedData" J..= sd
                                      ]
      Left e -> case (_selecteddata b) of
                  Nothing -> object [ "filterTitle" J..= (_filterTitle b)
                                    , "poisonsError" J..= e
                                    ]
                  Just sd -> object [ "filterTitle" J..= (_filterTitle b)
                                    , "poisonsError" J..= e
                                    , "selectedData" J..= sd
                                    ]

poisonsTopic :: Topic PoisonsModel
poisonsTopic = topic "poisons"

instance Default PoisonsModel where
  def :: PoisonsModel
  def = PoisonsModel
        { _filterTitle = ""
        , _poisons = Right []
        , _selecteddata = Nothing
        }

filterTitle :: Lens PoisonsModel MisoString
filterTitle = lens _filterTitle $ \m x -> m { _filterTitle = x}

poisons :: Lens PoisonsModel (Either MisoString [Poison])
poisons = lens _poisons $ \m x -> m { _poisons = x}

selecteddata :: Lens PoisonsModel (Maybe String)
selecteddata = lens _selecteddata $ \m x -> m { _selecteddata = x}

updateModel :: Action -> Effect a props PoisonsModel Action
updateModel GetPoisons       = getText "./data/poisons.json" [] SetPoisons ErrorHandler
updateModel (SetPoisons r)   = poisons .= (eitherDecode (body r)) >> issue PostPoisons
updateModel PostPoisons      = get >>= io_ . publish poisonsTopic
updateModel (ErrorHandler r) = maybe (issue $ ErrorUpdate "") (issue . ErrorUpdate) (errorMessage r)
updateModel (ErrorUpdate s)  = mailParent s >> io_ (print $ "Error: " <> s)
updateModel (UpdateFilter s) = filterTitle .= (fromMisoString s) >> issue PostPoisons
updateModel (SetPage s)      = selecteddata .= Just s

viewModel :: props -> PoisonsModel -> View PoisonsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col" ]
  [ banner Poisons
  , filterView m
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ] (map poisonView (filteredPoisons m))
  ]

filterView :: PoisonsModel -> View PoisonsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredPoisons :: PoisonsModel -> [Poison]
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

poisonView :: Poison -> View PoisonsModel Action
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

descriptionView :: Poison -> [View PoisonsModel Action]
descriptionView p = map renderStructure (p ^. description)

poisonsComponent :: PoisonsModel -> Component parent props PoisonsModel Action
poisonsComponent x = (vcomp x updateModel viewModel) { mount = if ( hasData $ _poisons x ) then Nothing else Just GetPoisons }
