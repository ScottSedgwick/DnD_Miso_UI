module Components.MagicItems
  ( MagicItemsModel(..)
  , magicItemsComponent
  , magicItemsTopic
  ) where

import           Data.Default        ( Default, def )
import           Miso                ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import qualified Miso.CSS            as MC
import           Miso.Fetch          ( Response(body, errorMessage), getText )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import           Miso.JSON            ( FromJSON, ToJSON, (.:), (.:?), eitherDecode, object, parseJSON, toJSON, withObject )
import qualified Miso.JSON            as J
import           Miso.Lens            (Lens, (.=), (^.), lens)
import           Miso.PubSub          ( Topic, topic )

import           Components.MagicItems.Model
import           Common.Accordion     ( accordion_, accordionSection_, accordionHeader_, accordionBody_)
import           Common.Banner        ( banner )
import           Common.Eithers       ( hasData )
import           Common.Pages         ( Page(..) )

data Action
  = GetMagicItems
  | DecodeMagicItems (Response MisoString)
  | SetMagicItems (Either MisoString [MagicItem])
  | PostMagicItems
  | ErrorHandler (Response MisoString)
  | UpdateFilter MisoString

data MagicItemsModel = MagicItemsModel
  { _magicItems :: Either MisoString [MagicItem]
  , _filterTitle :: MisoString
  } deriving (Show, Eq)
instance FromJSON MagicItemsModel where
  parseJSON =
    withObject "MagicItemsModel" $ \o -> do
      mi <- o .:? "magicItems"
      f <- o .: "filter"
      case mi of
        Just x -> pure $ MagicItemsModel { _magicItems = Right x, _filterTitle = f }
        Nothing -> do
          be <- o .:? "magicItemsError"
          case be of
            Just e -> pure $ MagicItemsModel { _magicItems = Left e, _filterTitle = f }
            Nothing -> pure $ MagicItemsModel { _magicItems = Right [], _filterTitle = f }
instance ToJSON MagicItemsModel where
  toJSON b =
    case (_magicItems b) of
      Right bs -> object [ "filter" J..= (_filterTitle b)
                         , "magicItems" J..= bs
                         ]
      Left e -> object [ "filter" J..= (_filterTitle b)
                       , "magicItemsError" J..= e
                       ]

magicItemsTopic :: Topic MagicItemsModel
magicItemsTopic = topic "magicItems"

magicItems :: Lens MagicItemsModel (Either MisoString [MagicItem])
magicItems = lens _magicItems $ \m x -> m { _magicItems = x }

filterTitle :: Lens MagicItemsModel MisoString
filterTitle = lens _filterTitle $ \m x -> m { _filterTitle = x }

instance Default MagicItemsModel where
  def :: MagicItemsModel
  def = MagicItemsModel
      { _magicItems = Right []
      , _filterTitle = ""
      }

updateModel :: Action -> Effect a props MagicItemsModel Action
updateModel GetMagicItems         = getText "./data/magicitems.json" [] DecodeMagicItems ErrorHandler
updateModel (DecodeMagicItems r)  = issue $ SetMagicItems (eitherDecode (body r))
updateModel (SetMagicItems r)     = magicItems .= r >> issue PostMagicItems
updateModel PostMagicItems        = get >>= (io_ . publish magicItemsTopic)
updateModel (ErrorHandler r)      = maybe (pure ()) mailParent (errorMessage r)
updateModel (UpdateFilter s)     = filterTitle .= (fromMisoString s) >> issue PostMagicItems

viewModel :: props -> MagicItemsModel -> View MagicItemsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col"]
  [ banner MagicItems
    , filterView m
    , H.div_ [ P.class_ "overflow-y-auto flex-1" ] [ text $ ms $ show $ length $ filteredMagicItems m ]
    , H.div_ [ P.class_ "overflow-y-auto flex-1" ]
      ( case (m ^. magicItems) of
          Right _ -> (map magicItemView (filteredMagicItems m))
          Left  e -> [ text $ "Error: [" <> e <> "]" ]
      )
  ]

filterView :: MagicItemsModel -> View MagicItemsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4" ]
  [ H.input_ [ P.placeholder_ "Filter", P.class_ "input", P.type_ "text", P.value_ (m ^. filterTitle), E.onInput UpdateFilter ]
  ]

filteredMagicItems :: MagicItemsModel -> [MagicItem]
filteredMagicItems m =
  case (m ^. magicItems) of
    (Left _  ) -> []
    (Right xs) -> xs

magicItemView :: MagicItem -> View MagicItemsModel Action
magicItemView m =
  accordion_ []
  [ accordionSection_ [ P.class_ "border-b" ]
    [ accordionHeader_ [] [ headerView m ]
    , accordionBody_ []
      [ H.section_ [ P.class_ "w-full rounded-lg border scroll-mt-14" ]
        [ H.div_ [ P.class_ "p-4" ]
          ( bodyView m
          )
        ]
      ]
    ]
  ]

headerView :: MagicItem -> View MagicItemsModel Action
headerView m =
  H.table_ [ MC.style_ [ MC.width "100%" ] ]
  [ H.tr_ [ MC.style_ [ MC.width "100%" ] ]
    [ H.td_ [ MC.style_ [ MC.width "33%" ] ] [ H.strong_ [] [ text "Title" ] ]
    , H.td_ [ MC.style_ [ MC.width "33%" ] ] [ H.strong_ [] [ text "Rarity" ] ]
    , H.td_ [ MC.style_ [ MC.width "33%" ] ] [ H.strong_ [] [ text "Type" ] ]
    ]
  , H.tr_ [ MC.style_ [ MC.width "100%" ] ]
    [ H.td_ [ MC.style_ [ MC.width "33%" ] ] [ text (ms $ _title m) ]
    , H.td_ [ MC.style_ [ MC.width "33%" ] ] [ text (ms $ show $ _rarity m) ]
    , H.td_ [ MC.style_ [ MC.width "33%" ] ] [ text (ms $ show $ _itemtype m) ]
    ]
  ]

  -- data MagicItem = MagicItem
  --   { _title :: MisoString
  --   , _url :: MisoString
  --   , _rarity :: Rarity
  --   , _itemtype :: ItemType
  --   , _attunement :: Attunement
  --   , _source :: [SourceBook]
  --   , _description :: [Structure]

bodyView :: MagicItem -> [View MagicItemsModel Action]
bodyView m =
  [ H.hr_ []
  ]

magicItemsComponent :: MagicItemsModel -> Component parent props MagicItemsModel Action
magicItemsComponent x = (vcomp x updateModel viewModel) { mount = if ( hasData $ _magicItems x ) then Nothing else Just GetMagicItems }
