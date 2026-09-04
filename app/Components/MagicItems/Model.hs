module Components.MagicItems.Model where

import           Data.Default        ( Default, def )
import           Miso                ( MisoString, fromMisoString )
import           Miso.JSON           ( FromJSON, ToJSON, Parser, Value(..), (.:), (.:?), object, parseJSON, toJSON, withObject )
import qualified Miso.JSON           as J
import           Miso.Lens            (Lens, lens)

import           Common.Structure    ( Structure )

data Rarity = RarityCommon
            | RarityUncommon
            | RarityRare
            | RarityVeryRare
            | RarityLegendary
            | RarityArtifact
            | RarityUnique
            | RarityUnknown MisoString
            deriving (Eq, Ord)
instance Show Rarity where
  show RarityCommon      = "Common"
  show RarityUncommon    = "Uncommon"
  show RarityRare        = "Rare"
  show RarityVeryRare    = "VeryRare"
  show RarityLegendary   = "Legendary"
  show RarityArtifact    = "Artifact"
  show RarityUnique      = "Unique"
  show (RarityUnknown _) = "Unknown"
instance Enum Rarity where
  fromEnum RarityCommon      = 0
  fromEnum RarityUncommon    = 1
  fromEnum RarityRare        = 2
  fromEnum RarityVeryRare    = 3
  fromEnum RarityLegendary   = 4
  fromEnum RarityArtifact    = 5
  fromEnum RarityUnique      = 6
  fromEnum (RarityUnknown _) = 7
  toEnum 0 = RarityCommon
  toEnum 1 = RarityUncommon
  toEnum 2 = RarityRare
  toEnum 3 = RarityVeryRare
  toEnum 4 = RarityLegendary
  toEnum 5 = RarityArtifact
  toEnum 6 = RarityUnique
  toEnum _ = (RarityUnknown "Unknown")
instance Bounded Rarity where
  minBound = RarityCommon
  maxBound = (RarityUnknown "Unknown")
instance FromJSON Rarity where
  parseJSON :: Value -> Parser Rarity
  parseJSON (String "common")    = pure RarityCommon
  parseJSON (String "uncommon")  = pure RarityUncommon
  parseJSON (String "rare")      = pure RarityRare
  parseJSON (String "veryrare")  = pure RarityVeryRare
  parseJSON (String "legendary") = pure RarityLegendary
  parseJSON (String "artifact")  = pure RarityArtifact
  parseJSON (String "unique")    = pure RarityUnique
  parseJSON (String s)           = pure $ RarityUnknown s
  parseJSON x                    = error $ "What is this?" <> show x
instance ToJSON Rarity where
  toJSON RarityCommon = (String "common")
  toJSON RarityUncommon = (String "uncommon")
  toJSON RarityRare = (String "rare")
  toJSON RarityVeryRare = (String "veryrare")
  toJSON RarityLegendary = (String "legendary")
  toJSON RarityArtifact = (String "artifact")
  toJSON RarityUnique = (String "unique")
  toJSON (RarityUnknown s) = (String s)

data ItemType = ItemTypeArmour (Maybe MisoString)
              | ItemTypeItem (Maybe MisoString)
              | ItemTypePotion (Maybe MisoString)
              | ItemTypeRing (Maybe MisoString)
              | ItemTypeRod (Maybe MisoString)
              | ItemTypeScroll (Maybe MisoString)
              | ItemTypeShield (Maybe MisoString)
              | ItemTypeStaff (Maybe MisoString)
              | ItemTypeWand (Maybe MisoString)
              | ItemTypeWeapon (Maybe MisoString)
              deriving (Eq)
instance Show ItemType where
  show (ItemTypeArmour (Just s)) = "Armour (" <> fromMisoString s <> ")"
  show (ItemTypeArmour Nothing) = "Armour"
  show (ItemTypeItem (Just s)) = "Item (" <> fromMisoString s <> ")"
  show (ItemTypeItem Nothing) = "Item"
  show (ItemTypePotion (Just s)) = "Potion (" <> fromMisoString s <> ")"
  show (ItemTypePotion Nothing) = "Potion"
  show (ItemTypeRing (Just s)) = "Ring (" <> fromMisoString s <> ")"
  show (ItemTypeRing Nothing) = "Ring"
  show (ItemTypeRod (Just s)) = "Rod (" <> fromMisoString s <> ")"
  show (ItemTypeRod Nothing) = "Rod"
  show (ItemTypeScroll (Just s)) = "Scroll (" <> fromMisoString s <> ")"
  show (ItemTypeScroll Nothing) = "Scroll"
  show (ItemTypeShield (Just s)) = "Shield (" <> fromMisoString s <> ")"
  show (ItemTypeShield Nothing) = "Shield"
  show (ItemTypeStaff (Just s)) = "Staff (" <> fromMisoString s <> ")"
  show (ItemTypeStaff Nothing) = "Staff"
  show (ItemTypeWand (Just s)) = "Wand (" <> fromMisoString s <> ")"
  show (ItemTypeWand Nothing) = "Wand"
  show (ItemTypeWeapon (Just s)) = "Weapon (" <> fromMisoString s <> ")"
  show (ItemTypeWeapon Nothing) = "Weapon"
instance Enum ItemType where
  fromEnum (ItemTypeArmour _) = 0
  fromEnum (ItemTypeItem _)   = 1
  fromEnum (ItemTypePotion _) = 2
  fromEnum (ItemTypeRing _)   = 3
  fromEnum (ItemTypeRod _)    = 4
  fromEnum (ItemTypeScroll _) = 5
  fromEnum (ItemTypeShield _) = 6
  fromEnum (ItemTypeStaff _)  = 7
  fromEnum (ItemTypeWand _)   = 8
  fromEnum (ItemTypeWeapon _) = 9
  toEnum 0 = ItemTypeArmour Nothing
  toEnum 1 = ItemTypeItem Nothing
  toEnum 2 = ItemTypePotion Nothing
  toEnum 3 = ItemTypeRing Nothing
  toEnum 4 = ItemTypeRod Nothing
  toEnum 5 = ItemTypeScroll Nothing
  toEnum 6 = ItemTypeShield Nothing
  toEnum 7 = ItemTypeStaff Nothing
  toEnum 8 = ItemTypeWand Nothing
  toEnum _ = ItemTypeWeapon Nothing
instance Bounded ItemType where
  minBound = ItemTypeArmour Nothing
  maxBound = ItemTypeWeapon Nothing
instance FromJSON ItemType where
  parseJSON :: Value -> Parser ItemType
  parseJSON = withObject "ItemType" $ \o -> do
    t <- o .: "type"
    d <- o .:? "detail"
    case t of
      "armour" -> pure $ maybe (ItemTypeArmour Nothing) (ItemTypeArmour . Just) d
      "item"   -> pure $ maybe (ItemTypeItem Nothing) (ItemTypeItem . Just) d
      "potion" -> pure $ maybe (ItemTypePotion Nothing) (ItemTypePotion . Just) d
      "ring"   -> pure $ maybe (ItemTypeRing Nothing) (ItemTypeRing . Just) d
      "rod"    -> pure $ maybe (ItemTypeRod Nothing) (ItemTypeRod . Just) d
      "scroll" -> pure $ maybe (ItemTypeScroll Nothing) (ItemTypeScroll . Just) d
      "shield" -> pure $ maybe (ItemTypeShield Nothing) (ItemTypeShield . Just) d
      "staff"  -> pure $ maybe (ItemTypeStaff Nothing) (ItemTypeStaff . Just) d
      "wand"   -> pure $ maybe (ItemTypeWand Nothing) (ItemTypeWand . Just) d
      "weapon" -> pure $ maybe (ItemTypeWeapon Nothing) (ItemTypeWeapon . Just) d
      _ -> error $ "Invalid item type: " <> t
instance ToJSON ItemType where
  toJSON (ItemTypeArmour Nothing)  = object [ "type" J..= (String "armour")]
  toJSON (ItemTypeArmour (Just s)) = object [ "type" J..= (String "armour"), "detail" J..= s]
  toJSON (ItemTypeItem Nothing)    = object [ "type" J..= (String "item")]
  toJSON (ItemTypeItem (Just s))   = object [ "type" J..= (String "item"), "detail" J..= s]
  toJSON (ItemTypePotion Nothing)  = object [ "type" J..= (String "potion")]
  toJSON (ItemTypePotion (Just s)) = object [ "type" J..= (String "potion"), "detail" J..= s]
  toJSON (ItemTypeRing Nothing)    = object [ "type" J..= (String "ring")]
  toJSON (ItemTypeRing (Just s))   = object [ "type" J..= (String "ring"), "detail" J..= s]
  toJSON (ItemTypeRod Nothing)     = object [ "type" J..= (String "rod")]
  toJSON (ItemTypeRod (Just s))    = object [ "type" J..= (String "rod"), "detail" J..= s]
  toJSON (ItemTypeScroll Nothing)  = object [ "type" J..= (String "scroll")]
  toJSON (ItemTypeScroll (Just s)) = object [ "type" J..= (String "scroll"), "detail" J..= s]
  toJSON (ItemTypeShield Nothing)  = object [ "type" J..= (String "shield")]
  toJSON (ItemTypeShield (Just s)) = object [ "type" J..= (String "shield"), "detail" J..= s]
  toJSON (ItemTypeStaff Nothing)   = object [ "type" J..= (String "staff")]
  toJSON (ItemTypeStaff (Just s))  = object [ "type" J..= (String "staff"), "detail" J..= s]
  toJSON (ItemTypeWand Nothing)    = object [ "type" J..= (String "wand")]
  toJSON (ItemTypeWand (Just s))   = object [ "type" J..= (String "wand"), "detail" J..= s]
  toJSON (ItemTypeWeapon Nothing)  = object [ "type" J..= (String "weapon")]
  toJSON (ItemTypeWeapon (Just s)) = object [ "type" J..= (String "weapon"), "detail" J..= s]

data Attunement = AttuneNone
                | Attune (Maybe MisoString)
                deriving (Show, Eq)
instance FromJSON Attunement where
  parseJSON :: Value -> Parser Attunement
  parseJSON = withObject "Attunement" $ \o -> do
    a <- o .: "attune"
    d <- o .:? "detail"
    pure $ if a then (maybe (Attune Nothing) (Attune . Just) d) else AttuneNone
instance ToJSON Attunement where
  toJSON AttuneNone        = object [ "attune" J..= False ]
  toJSON (Attune Nothing)  = object [ "attune" J..= True ]
  toJSON (Attune (Just s)) = object [ "attune" J..= True, "detail" J..= s ]

data SourceBook
  = SourceAcquisitionsIncorporated
  | SourceBaldursGateDescentIntoAvernus
  | SourceBigbyPresentsGloryOfTheGiants
  | SourceBookOfManyThings
  | SourceCandlekeepMysteries
  | SourceCriticalRoleCallOfNetherdeep
  | SourceCurseOfStrahd
  | SourceDivineContention
  | SourceDMG
  | SourceDragonlanceShadowOfTheDragonQueen
  | SourceDungeonsAndDragonsHonorAmongThieves
  | SourceEberronRisingFromTheLastWar
  | SourceExplorersGuideToWildemount
  | SourceFizbansTreasuryOfDragons
  | SourceGhostsOfSaltmarsh
  | SourceGuildmastersGuideToRavnica
  | SourceIcewindDaleRimeOfTheFrostmaiden
  | SourceInfernalMachineRebuild
  | SourceJourneysThroughTheRadiantCitadel
  | SourceKeysFromTheGoldenVault
  | SourceLostLaboratoryOfKwalish
  | SourceLostMineOfPhandelver
  | SourceMonstrousCompendium2
  | SourceMythicOdysseysOfTheros
  | SourceOutOfTheAbyss
  | SourcePhandelverAndBelowTheShatteredObelisk
  | SourcePlanescapeAdventuresInTheMultiverse
  | SourcePrincesOfTheApocalypse
  | SourceQuestsFromTheInfiniteStaircase
  | SourceSleepingDragonsWake
  | SourceSpelljammerAdventuresInSpace
  | SourceStormKingsThunder
  | SourceStrixhavenCurriculumOfChaos
  | SourceTalesFromTheYawningPortal
  | SourceTashasCauldronOfEverything
  | SourceTheRiseOfTiamat
  | SourceTheWildBeyondTheWitchlight
  | SourceTombOfAnnihilation
  | SourceTyrannyOfDragons
  | SourceVanRichtensGuideToRavenloft
  | SourceVecnaEyeOfRuin
  | SourceVolosGuideToMonsters
  | SourceWaterdeepDragonHeist
  | SourceWaterdeepDungeonOfTheMadMage
  | SourceWayfarersGuideToEberron
  | SourceXanatharsGuideToEverything
  | SourceUnknown MisoString
  deriving (Show, Eq)
instance FromJSON SourceBook where
  parseJSON :: Value -> Parser SourceBook
  parseJSON (String "AcquisitionsIncorporated") = pure $ SourceAcquisitionsIncorporated
  parseJSON (String "BaldursGateDescentIntoAvernus") = pure $ SourceBaldursGateDescentIntoAvernus
  parseJSON (String "BigbyPresentsGloryOfTheGiants") = pure $ SourceBigbyPresentsGloryOfTheGiants
  parseJSON (String "BookOfManyThings") = pure $ SourceBookOfManyThings
  parseJSON (String "CandlekeepMysteries") = pure $ SourceCandlekeepMysteries
  parseJSON (String "CriticalRoleCallOfNetherdeep") = pure $ SourceCriticalRoleCallOfNetherdeep
  parseJSON (String "CurseOfStrahd") = pure $ SourceCurseOfStrahd
  parseJSON (String "DivineContention") = pure $ SourceDivineContention
  parseJSON (String "DMG") = pure $ SourceDMG
  parseJSON (String "DragonlanceShadowOfTheDragonQueen") = pure SourceDragonlanceShadowOfTheDragonQueen
  parseJSON (String "DungeonsAndDragonsHonorAmongThieves") = pure SourceDungeonsAndDragonsHonorAmongThieves
  parseJSON (String "EberronRisingFromTheLastWar") = pure $ SourceEberronRisingFromTheLastWar
  parseJSON (String "ExplorersGuideToWildemount") = pure $ SourceExplorersGuideToWildemount
  parseJSON (String "FizbansTreasuryOfDragons") = pure $ SourceFizbansTreasuryOfDragons
  parseJSON (String "GhostsOfSaltmarsh") = pure $ SourceGhostsOfSaltmarsh
  parseJSON (String "GuildmastersGuideToRavnica") = pure $ SourceGuildmastersGuideToRavnica
  parseJSON (String "IcewindDaleRimeOfTheFrostmaiden") = pure $ SourceIcewindDaleRimeOfTheFrostmaiden
  parseJSON (String "InfernalMachineRebuild") = pure $ SourceInfernalMachineRebuild
  parseJSON (String "JourneysThroughTheRadiantCitadel") = pure $ SourceJourneysThroughTheRadiantCitadel
  parseJSON (String "KeysFromTheGoldenVault") = pure $ SourceKeysFromTheGoldenVault
  parseJSON (String "LostLaboratoryOfKwalish") = pure $ SourceLostLaboratoryOfKwalish
  parseJSON (String "LostMineOfPhandelver") = pure $ SourceLostMineOfPhandelver
  parseJSON (String "MonstrousCompendium2") = pure $ SourceMonstrousCompendium2
  parseJSON (String "MythicOdysseysOfTheros") = pure $ SourceMythicOdysseysOfTheros
  parseJSON (String "OutOfTheAbyss") = pure $ SourceOutOfTheAbyss
  parseJSON (String "PhandelverAndBelowTheShatteredObelisk") = pure $ SourcePhandelverAndBelowTheShatteredObelisk
  parseJSON (String "PlanescapeAdventuresInTheMultiverse") = pure $ SourcePlanescapeAdventuresInTheMultiverse
  parseJSON (String "PrincesOfTheApocalypse") = pure $ SourcePrincesOfTheApocalypse
  parseJSON (String "QuestsFromTheInfiniteStaircase") = pure $ SourceQuestsFromTheInfiniteStaircase
  parseJSON (String "SleepingDragonsWake") = pure $ SourceSleepingDragonsWake
  parseJSON (String "SpelljammerAdventuresInSpace") = pure $ SourceSpelljammerAdventuresInSpace
  parseJSON (String "StormKingsThunder") = pure $ SourceStormKingsThunder
  parseJSON (String "StrixhavenCurriculumOfChaos") = pure $ SourceStrixhavenCurriculumOfChaos
  parseJSON (String "TalesFromTheYawningPortal") = pure $ SourceTalesFromTheYawningPortal
  parseJSON (String "TashasCauldronOfEverything") = pure $ SourceTashasCauldronOfEverything
  parseJSON (String "TheRiseOfTiamat") = pure $ SourceTheRiseOfTiamat
  parseJSON (String "TheWildBeyondTheWitchlight") = pure $ SourceTheWildBeyondTheWitchlight
  parseJSON (String "TombOfAnnihilation") = pure $ SourceTombOfAnnihilation
  parseJSON (String "TyrannyOfDragons") = pure $ SourceTyrannyOfDragons
  parseJSON (String "VanRichtensGuideToRavenloft") = pure $ SourceVanRichtensGuideToRavenloft
  parseJSON (String "VecnaEyeOfRuin") = pure $ SourceVecnaEyeOfRuin
  parseJSON (String "VolosGuideToMonsters") = pure $ SourceVolosGuideToMonsters
  parseJSON (String "WaterdeepDragonHeist") = pure $ SourceWaterdeepDragonHeist
  parseJSON (String "WaterdeepDungeonOfTheMadMage") = pure $ SourceWaterdeepDungeonOfTheMadMage
  parseJSON (String "WayfarersGuideToEberron") = pure $ SourceWayfarersGuideToEberron
  parseJSON (String "XanatharsGuideToEverything") = pure $ SourceXanatharsGuideToEverything
  parseJSON (String s) = pure $ SourceUnknown s
  parseJSON x = error $ "What kind of source book is that?" <> show x
instance ToJSON SourceBook where
  toJSON SourceAcquisitionsIncorporated = (String "AcquisitionsIncorporated")
  toJSON SourceBaldursGateDescentIntoAvernus = (String "BaldursGateDescentIntoAvernus")
  toJSON SourceBigbyPresentsGloryOfTheGiants = (String "BigbyPresentsGloryOfTheGiants")
  toJSON SourceBookOfManyThings = (String "BookOfManyThings")
  toJSON SourceCandlekeepMysteries = (String "CandlekeepMysteries")
  toJSON SourceCriticalRoleCallOfNetherdeep = (String "CriticalRoleCallOfNetherdeep")
  toJSON SourceCurseOfStrahd = (String "CurseOfStrahd")
  toJSON SourceDivineContention = (String "DivineContention")
  toJSON SourceDMG = (String "DMG")
  toJSON SourceDragonlanceShadowOfTheDragonQueen = (String "DragonlanceShadowOfTheDragonQueen")
  toJSON SourceDungeonsAndDragonsHonorAmongThieves = (String "DungeonsAndDragonsHonorAmongThieves")
  toJSON SourceEberronRisingFromTheLastWar = (String "EberronRisingFromTheLastWar")
  toJSON SourceExplorersGuideToWildemount = (String "ExplorersGuideToWildemount")
  toJSON SourceFizbansTreasuryOfDragons = (String "FizbansTreasuryOfDragons")
  toJSON SourceGhostsOfSaltmarsh = (String "GhostsOfSaltmarsh")
  toJSON SourceGuildmastersGuideToRavnica = (String "GuildmastersGuideToRavnica")
  toJSON SourceIcewindDaleRimeOfTheFrostmaiden = (String "IcewindDaleRimeOfTheFrostmaiden")
  toJSON SourceInfernalMachineRebuild = (String "InfernalMachineRebuild")
  toJSON SourceJourneysThroughTheRadiantCitadel = (String "JourneysThroughTheRadiantCitadel")
  toJSON SourceKeysFromTheGoldenVault = (String "KeysFromTheGoldenVault")
  toJSON SourceLostLaboratoryOfKwalish = (String "LostLaboratoryOfKwalish")
  toJSON SourceLostMineOfPhandelver = (String "LostMineOfPhandelver")
  toJSON SourceMonstrousCompendium2 = (String "MonstrousCompendium2")
  toJSON SourceMythicOdysseysOfTheros = (String "MythicOdysseysOfTheros")
  toJSON SourceOutOfTheAbyss = (String "OutOfTheAbyss")
  toJSON SourcePhandelverAndBelowTheShatteredObelisk = (String "PhandelverAndBelowTheShatteredObelisk")
  toJSON SourcePlanescapeAdventuresInTheMultiverse = (String "PlanescapeAdventuresInTheMultiverse")
  toJSON SourcePrincesOfTheApocalypse = (String "PrincesOfTheApocalypse")
  toJSON SourceQuestsFromTheInfiniteStaircase = (String "QuestsFromTheInfiniteStaircase")
  toJSON SourceSleepingDragonsWake = (String "SleepingDragonsWake")
  toJSON SourceSpelljammerAdventuresInSpace = (String "SpelljammerAdventuresInSpace")
  toJSON SourceStormKingsThunder = (String "StormKingsThunder")
  toJSON SourceStrixhavenCurriculumOfChaos = (String "StrixhavenCurriculumOfChaos")
  toJSON SourceTalesFromTheYawningPortal = (String "TalesFromTheYawningPortal")
  toJSON SourceTashasCauldronOfEverything = (String "TashasCauldronOfEverything")
  toJSON SourceTheRiseOfTiamat = (String "TheRiseOfTiamat")
  toJSON SourceTheWildBeyondTheWitchlight = (String "TheWildBeyondTheWitchlight")
  toJSON SourceTombOfAnnihilation = (String "TombOfAnnihilation")
  toJSON SourceTyrannyOfDragons = (String "TyrannyOfDragons")
  toJSON SourceVanRichtensGuideToRavenloft = (String "VanRichtensGuideToRavenloft")
  toJSON SourceVecnaEyeOfRuin = (String "VecnaEyeOfRuin")
  toJSON SourceVolosGuideToMonsters = (String "VolosGuideToMonsters")
  toJSON SourceWaterdeepDragonHeist = (String "WaterdeepDragonHeist")
  toJSON SourceWaterdeepDungeonOfTheMadMage = (String "WaterdeepDungeonOfTheMadMage")
  toJSON SourceWayfarersGuideToEberron = (String "WayfarersGuideToEberron")
  toJSON SourceXanatharsGuideToEverything = (String "XanatharsGuideToEverything")
  toJSON (SourceUnknown s) = (String s)

data MagicItem = MagicItem
  { _title :: MisoString
  , _url :: MisoString
  , _rarity :: [Rarity]
  , _itemtype :: ItemType
  , _attunement :: Attunement
  , _source :: [SourceBook]
  , _description :: [Structure]
  } deriving (Show, Eq)

instance FromJSON MagicItem where
  parseJSON :: Value -> Parser MagicItem
  parseJSON = withObject "MagicItem" $ \o -> do
    t <- o .: "title"
    u <- o .: "url"
    r <- o .: "rarity"
    i <- o .: "itemtype"
    a <- o .: "attunement"
    s <- o .: "source"
    d <- o .: "description"
    pure $ MagicItem
      { _title = t
      , _url = u
      , _rarity = r
      , _itemtype = i
      , _attunement = a
      , _source = s
      , _description = d
      }
instance ToJSON MagicItem where
  toJSON m = object [ "title" J..= (_title m)
                    , "url" J..= (_url m)
                    , "rarity" J..= (_rarity m)
                    , "itemtype" J..= (_itemtype m)
                    , "attunement" J..= (_attunement m)
                    , "source" J..= (_source m)
                    , "description" J..= (_description m)
                    ]

data MagicItemFilter = MagicItemFilter
  { _flt_title :: MisoString
  , _flt_type :: Maybe ItemType
  , _flt_rarity :: Maybe Rarity
  } deriving (Show, Eq)

instance FromJSON MagicItemFilter where
  parseJSON :: Value -> Parser MagicItemFilter
  parseJSON = withObject "MagicItemFilter" $ \o -> do
    t <- o .: "title"
    y <- o .:? "type"
    r <- o .:? "rarity"
    pure $ MagicItemFilter
      { _flt_title = t
      , _flt_type = y
      , _flt_rarity = r
      }
instance ToJSON MagicItemFilter where
  toJSON m = do
    case (_flt_type m) of
      Nothing -> case (_flt_rarity m) of
                  Nothing -> object [ "title" J..= (_flt_title m) ]
                  Just r  -> object [ "title" J..= (_flt_title m), "rarity" J..= r ]
      Just y  -> case (_flt_rarity m) of
                  Nothing -> object [ "title" J..= (_flt_title m), "type" J..= y ]
                  Just r  -> object [ "title" J..= (_flt_title m), "type" J..= y, "rarity" J..= r ]
instance Default MagicItemFilter where
  def = MagicItemFilter
        { _flt_title = ""
        , _flt_type = Nothing
        , _flt_rarity = Nothing
        }

flt_title :: Lens MagicItemFilter MisoString
flt_title = lens _flt_title $ \m x -> m { _flt_title = x }

flt_type :: Lens MagicItemFilter (Maybe ItemType)
flt_type = lens _flt_type $ \m x -> m { _flt_type = x }

flt_rarity :: Lens MagicItemFilter (Maybe Rarity)
flt_rarity = lens _flt_rarity $ \m x -> m { _flt_rarity = x }

data MagicItemsModel = MagicItemsModel
  { _magicItems :: Either MisoString [MagicItem]
  , _itemFilter :: MagicItemFilter
  } deriving (Show, Eq)
instance FromJSON MagicItemsModel where
  parseJSON =
    withObject "MagicItemsModel" $ \o -> do
      mi <- o .:? "magicItems"
      f <- o .: "filter"
      case mi of
        Just x -> pure $ MagicItemsModel { _magicItems = Right x, _itemFilter = f }
        Nothing -> do
          be <- o .:? "magicItemsError"
          case be of
            Just e -> pure $ MagicItemsModel { _magicItems = Left e, _itemFilter = f }
            Nothing -> pure $ MagicItemsModel { _magicItems = Right [], _itemFilter = f }
instance ToJSON MagicItemsModel where
  toJSON b =
    case (_magicItems b) of
      Right bs -> object [ "filter" J..= (_itemFilter b)
                          , "magicItems" J..= bs
                          ]
      Left e -> object [ "filter" J..= (_itemFilter b)
                        , "magicItemsError" J..= e
                        ]

instance Default MagicItemsModel where
  def :: MagicItemsModel
  def = MagicItemsModel
      { _magicItems = Right []
      , _itemFilter = def
      }

magicItems :: Lens MagicItemsModel (Either MisoString [MagicItem])
magicItems = lens _magicItems $ \m x -> m { _magicItems = x }

itemFilter :: Lens MagicItemsModel MagicItemFilter
itemFilter = lens _itemFilter $ \m x -> m { _itemFilter = x }
