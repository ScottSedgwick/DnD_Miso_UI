module Components.MagicItems.Model where

import           Miso                ( MisoString )
import           Miso.JSON            ( FromJSON, ToJSON, Parser, Value(..), (.:), (.:?), object, parseJSON, toJSON, withObject )
import qualified Miso.JSON            as J

import           Common.Structure     ( Structure )

data Rarity = RarityCommon
            | RarityUncommon
            | RarityRare
            | RarityVeryRare
            | RarityLegendary
            | RarityArtifact
            | RarityUnknown MisoString
            deriving (Show, Eq)
instance FromJSON Rarity where
  parseJSON :: Value -> Parser Rarity
  parseJSON (String "common")    = pure RarityCommon
  parseJSON (String "uncommon")  = pure RarityUncommon
  parseJSON (String "rare")      = pure RarityRare
  parseJSON (String "veryrare")  = pure RarityVeryRare
  parseJSON (String "legendary") = pure RarityLegendary
  parseJSON (String "artifact")  = pure RarityArtifact
  parseJSON (String s)           = pure $ RarityUnknown s
  parseJSON x                    = error $ "What is this?" <> show x
instance ToJSON Rarity where
  toJSON RarityCommon = (String "common")
  toJSON RarityUncommon = (String "uncommon")
  toJSON RarityRare = (String "rare")
  toJSON RarityVeryRare = (String "veryrare")
  toJSON RarityLegendary = (String "legendary")
  toJSON RarityArtifact = (String "artifact")
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
              deriving (Show, Eq)
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
  = SourceBaldursGateDescentIntoAvernus
  | SourceBigbyPresentsGloryOfTheGiants
  | SourceBookOfManyThings
  | SourceCandlekeepMysteries
  | SourceCriticalRoleCallOfNetherdeep
  | SourceCurseOfStrahd
  | SourceDMG
  | SourceEberronRisingFromTheLastWar
  | SourceExplorersGuideToWildemount
  | SourceFizbansTreasuryOfDragons
  | SourceGhostsOfSaltmarsh
  | SourceGuildmastersGuideToRavnica
  | SourceIcewindDaleRimeOfTheFrostmaiden
  | SourceInfernalMachineRebuild
  | SourceLostLaboratoryOfKwalish
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
  | SourceTheWildBeyondTheWitchlight
  | SourceTombOfAnnihilation
  | SourceTyrannyOfDragons
  | SourceVanRichtensGuideToRavenloft
  | SourceVolosGuideToMonsters
  | SourceWaterdeepDragonHeist
  | SourceWaterdeepDungeonOfTheMadMage
  | SourceWayfarersGuideToEberron
  | SourceXanatharsGuideToEverything
  | SourceUnknown MisoString
  deriving (Show, Eq)
instance FromJSON SourceBook where
  parseJSON :: Value -> Parser SourceBook
  parseJSON (String "BaldursGateDescentIntoAvernus") = pure $ SourceBaldursGateDescentIntoAvernus
  parseJSON (String "BigbyPresentsGloryOfTheGiants") = pure $ SourceBigbyPresentsGloryOfTheGiants
  parseJSON (String "BookOfManyThings") = pure $ SourceBookOfManyThings
  parseJSON (String "CandlekeepMysteries") = pure $ SourceCandlekeepMysteries
  parseJSON (String "CriticalRoleCallOfNetherdeep") = pure $ SourceCriticalRoleCallOfNetherdeep
  parseJSON (String "CurseOfStrahd") = pure $ SourceCurseOfStrahd
  parseJSON (String "DMG") = pure $ SourceDMG
  parseJSON (String "EberronRisingFromTheLastWar") = pure $ SourceEberronRisingFromTheLastWar
  parseJSON (String "ExplorersGuideToWildemount") = pure $ SourceExplorersGuideToWildemount
  parseJSON (String "FizbansTreasuryOfDragons") = pure $ SourceFizbansTreasuryOfDragons
  parseJSON (String "GhostsOfSaltmarsh") = pure $ SourceGhostsOfSaltmarsh
  parseJSON (String "GuildmastersGuideToRavnica") = pure $ SourceGuildmastersGuideToRavnica
  parseJSON (String "IcewindDaleRimeOfTheFrostmaiden") = pure $ SourceIcewindDaleRimeOfTheFrostmaiden
  parseJSON (String "InfernalMachineRebuild") = pure $ SourceInfernalMachineRebuild
  parseJSON (String "LostLaboratoryOfKwalish") = pure $ SourceLostLaboratoryOfKwalish
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
  parseJSON (String "TheWildBeyondTheWitchlight") = pure $ SourceTheWildBeyondTheWitchlight
  parseJSON (String "TombOfAnnihilation") = pure $ SourceTombOfAnnihilation
  parseJSON (String "TyrannyOfDragons") = pure $ SourceTyrannyOfDragons
  parseJSON (String "VanRichtensGuideToRavenloft") = pure $ SourceVanRichtensGuideToRavenloft
  parseJSON (String "VolosGuideToMonsters") = pure $ SourceVolosGuideToMonsters
  parseJSON (String "WaterdeepDragonHeist") = pure $ SourceWaterdeepDragonHeist
  parseJSON (String "WaterdeepDungeonOfTheMadMage") = pure $ SourceWaterdeepDungeonOfTheMadMage
  parseJSON (String "WayfarersGuideToEberron") = pure $ SourceWayfarersGuideToEberron
  parseJSON (String "XanatharsGuideToEverything") = pure $ SourceXanatharsGuideToEverything
  parseJSON (String s) = pure $ SourceUnknown s
  parseJSON x = error $ "What kind of source book is that?" <> show x
instance ToJSON SourceBook where
  toJSON SourceBaldursGateDescentIntoAvernus = (String "BaldursGateDescentIntoAvernus")
  toJSON SourceBigbyPresentsGloryOfTheGiants = (String "BigbyPresentsGloryOfTheGiants")
  toJSON SourceBookOfManyThings = (String "BookOfManyThings")
  toJSON SourceCandlekeepMysteries = (String "CandlekeepMysteries")
  toJSON SourceCriticalRoleCallOfNetherdeep = (String "CriticalRoleCallOfNetherdeep")
  toJSON SourceCurseOfStrahd = (String "CurseOfStrahd")
  toJSON SourceDMG = (String "DMG")
  toJSON SourceEberronRisingFromTheLastWar = (String "EberronRisingFromTheLastWar")
  toJSON SourceExplorersGuideToWildemount = (String "ExplorersGuideToWildemount")
  toJSON SourceFizbansTreasuryOfDragons = (String "FizbansTreasuryOfDragons")
  toJSON SourceGhostsOfSaltmarsh = (String "GhostsOfSaltmarsh")
  toJSON SourceGuildmastersGuideToRavnica = (String "GuildmastersGuideToRavnica")
  toJSON SourceIcewindDaleRimeOfTheFrostmaiden = (String "IcewindDaleRimeOfTheFrostmaiden")
  toJSON SourceInfernalMachineRebuild = (String "InfernalMachineRebuild")
  toJSON SourceLostLaboratoryOfKwalish = (String "LostLaboratoryOfKwalish")
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
  toJSON SourceTheWildBeyondTheWitchlight = (String "TheWildBeyondTheWitchlight")
  toJSON SourceTombOfAnnihilation = (String "TombOfAnnihilation")
  toJSON SourceTyrannyOfDragons = (String "TyrannyOfDragons")
  toJSON SourceVanRichtensGuideToRavenloft = (String "VanRichtensGuideToRavenloft")
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
