{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}
module Components.Insults where

import           Data.Default        ( Default, def )
import           Miso                ( Component (mount), Effect, MisoString, View, fromMisoString, get, io, io_, issue, mailParent, publish, vcomp )
import           Miso.Fetch          ( Response(body, errorMessage), getText )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import           Miso.JSON            ( FromJSON, Parser, Value, (.:?), (.!=), eitherDecode, parseJSON, withObject )
import           Miso.Lens            (Lens, (.=), (^.), lens)
import           Model.MailboxMessage ( insultsTopic, currentInsultTopic )
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
  | PostInsults (Either MisoString [MisoString])
  | PostCurrentInsult MisoString
  | ErrorHandler (Response MisoString)
  | GetNewInsult
  | SetNewInsult MisoString

data Model = Model
  { _insults :: Either MisoString [MisoString]
  , _currentInsult :: MisoString
  } deriving (Show, Eq)

insults :: Lens Model (Either MisoString [MisoString])
insults = lens _insults $ \m x -> m { _insults = x }

currentInsult :: Lens Model MisoString
currentInsult = lens _currentInsult $ \m x -> m { _currentInsult = x }

instance Default Model where
  def :: Model
  def = Model 
      { _insults = Right []
      , _currentInsult = ""
      }

updateModel :: Action -> Effect a Model Action
updateModel GetInsults            = getText "./data/insults.json" [] SetInsults ErrorHandler
updateModel (SetInsults r)        = let x = parseInsultResponse r in insults .= x >> issue (PostInsults x)
updateModel (PostInsults x)       = either (const $ pure ()) (io_ . publish insultsTopic) x >> issue GetNewInsult
updateModel (PostCurrentInsult s) = io_ $ publish currentInsultTopic s
updateModel (ErrorHandler r)      = maybe (pure ()) mailParent (errorMessage r)
updateModel (SetNewInsult s)      = currentInsult .= s >> issue (PostCurrentInsult s) >> io_ (print s)
updateModel GetNewInsult          = get >>= \m -> io $ do
  putStrLn "Getting New insult"
  case (m ^. insults) of
    Left _ -> pure (SetNewInsult "No Insults found")
    Right xs -> do
      putStrLn "Randomizing Insult"
      s <- pickRandom xs
      putStrLn $ "Random Insult: " <> fromMisoString s
      pure (SetNewInsult s)

parseInsultResponse :: Response MisoString -> Either MisoString [MisoString]
parseInsultResponse r =
  case eitherDecode (body r) :: Either MisoString InsultsJson of
    Left e -> Left e
    Right j -> Right (xs j)

pickRandom :: [a] -> IO a
pickRandom xs = do
  i <- randomRIO (0, length xs - 1)
  pure $ xs !! i

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [ P.class_ "h-screen flex flex-col"] 
  [ banner Insults
  , H.div_ [ P.class_ "overflow-y-auto flex-1" ]
    [ H.textarea_ [ P.placeholder_ "Type your message here", P.class_ "textarea", E.onClick GetNewInsult, P.readonly_ True, P.value_ (m ^. currentInsult) ] []
    ]
  ]

insultsComponent :: [MisoString] -> MisoString -> Component parent Model Action
insultsComponent xs curr = 
  if xs == []
    then
      (vcomp def updateModel viewModel) { mount = Just GetInsults }
    else
      (vcomp (def { _insults = Right xs, _currentInsult = curr }) updateModel viewModel)