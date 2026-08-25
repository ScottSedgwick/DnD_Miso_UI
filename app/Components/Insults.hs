module Components.Insults
  ( InsultsModel(..)
  , insultsComponent
  , insultsTopic
  ) where

import           Data.Default        ( Default, def )
import           Miso                ( Component (mount), Effect, MisoString, View, fromMisoString, get, io, io_, issue, mailParent, publish, vcomp )
import           Miso.Fetch          ( Response(body, errorMessage), getText )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import           Miso.JSON            ( FromJSON, ToJSON, Parser, Value, (.:), (.:?), (.!=), eitherDecode, object, parseJSON, toJSON, withObject )
import qualified Miso.JSON            as J
import           Miso.Lens            (Lens, (.=), (^.), lens)
import           Miso.PubSub          ( Topic, topic )
import           System.Random        ( randomRIO )

import           Common.Banner        ( banner )
import           Common.Pages         ( Page(..) )

data InsultsJson = InsultsJson { xs :: [MisoString] } deriving (Show, Eq)

instance FromJSON InsultsJson where
  parseJSON :: Value -> Parser InsultsJson
  parseJSON = withObject "Spell" $ \o -> do
    xs_ <- o .:? "insults" .!= []
    pure $ InsultsJson { xs = xs_ }

data Action
  = GetInsults
  | SetInsults (Response MisoString)
  | PostInsults
  | ErrorHandler (Response MisoString)
  | GetNewInsult
  | SetNewInsult MisoString

data InsultsModel = InsultsModel
  { _insults :: Either MisoString [MisoString]
  , _currentInsult :: MisoString
  } deriving (Show, Eq)
instance FromJSON InsultsModel where
  parseJSON =
    withObject "InsultsModel" $ \o -> do
      ci <- o .: "currentInsult"
      ms <- o .:? "insults"
      case ms of
        Just x -> pure $ InsultsModel { _currentInsult = ci, _insults = Right x }
        Nothing -> do
          be <- o .:? "insultsError"
          case be of
            Just e -> pure $ InsultsModel { _currentInsult = ci, _insults = Left e }
            Nothing -> pure $ InsultsModel { _currentInsult = ci, _insults = Right [] }
instance ToJSON InsultsModel where
  toJSON b =
    case (_insults b) of
      Right bs -> object [ "currentInsult" J..= (_currentInsult b)
                         , "insults" J..= bs
                         ]
      Left e -> object [ "currentInsult" J..= (_currentInsult b)
                       , "insultsError" J..= e
                       ]


insultsTopic :: Topic InsultsModel
insultsTopic = topic "insults"

insults :: Lens InsultsModel (Either MisoString [MisoString])
insults = lens _insults $ \m x -> m { _insults = x }

currentInsult :: Lens InsultsModel MisoString
currentInsult = lens _currentInsult $ \m x -> m { _currentInsult = x }

instance Default InsultsModel where
  def :: InsultsModel
  def = InsultsModel
      { _insults = Right []
      , _currentInsult = ""
      }

updateModel :: Action -> Effect a props InsultsModel Action
updateModel GetInsults            = getText "./data/insults.json" [] SetInsults ErrorHandler
updateModel (SetInsults r)        = let x = parseInsultResponse r in insults .= x >> issue GetNewInsult >> issue PostInsults
updateModel PostInsults           = get >>= (io_ . publish insultsTopic)
updateModel (ErrorHandler r)      = maybe (pure ()) mailParent (errorMessage r)
updateModel (SetNewInsult s)      = currentInsult .= s >> issue PostInsults >> io_ (print s)
updateModel GetNewInsult          = get >>= \m -> io $ do
  putStrLn "Getting New insult"
  case (m ^. insults) of
    Left _ -> pure (SetNewInsult "No Insults found")
    Right ins -> do
      putStrLn "Randomizing Insult"
      s <- pickRandom ins
      putStrLn $ "Random Insult: " <> fromMisoString s
      pure (SetNewInsult s)

parseInsultResponse :: Response MisoString -> Either MisoString [MisoString]
parseInsultResponse r =
  case eitherDecode (body r) :: Either MisoString InsultsJson of
    Left e -> Left e
    Right j -> Right (xs j)

pickRandom :: [a] -> IO a
pickRandom ins = do
  i <- randomRIO (0, length ins - 1)
  pure $ ins !! i

viewModel :: props -> InsultsModel -> View InsultsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col"]
  [ banner Insults
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ]
    [ H.textarea_ [ P.placeholder_ "Type your message here", P.class_ "textarea", E.onClick GetNewInsult, P.readonly_ True, P.value_ (m ^. currentInsult) ]
    ]
  ]

insultsComponent :: InsultsModel -> Component parent props InsultsModel Action
insultsComponent x =
  case (_insults x) of
    Right (_:_) -> (vcomp x updateModel viewModel)
    _ -> (vcomp x updateModel viewModel) { mount = Just GetInsults }
