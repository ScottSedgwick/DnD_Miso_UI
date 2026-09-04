module Components.MagicItems
  ( MagicItemsModel(..)
  , magicItemsComponent
  , magicItemsTopic
  ) where

import qualified Data.Char           as C
import qualified Data.List           as L
import           Miso                ( Component (mount), Effect, MisoString, View, fromMisoString, get, io_, issue, mailParent, ms, publish, text, vcomp )
import qualified Miso.CSS            as MC
import           Miso.Fetch          ( Response(body, errorMessage), getText )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import           Miso.JSON            ( eitherDecode )
import           Miso.Lens            ( (.=), (^.) )
import           Miso.String          ( intercalate )
import           Miso.PubSub          ( Topic, topic )

import           Components.MagicItems.Model
import           Common.Accordion     ( accordion_, accordionSection_, accordionHeader_, accordionBody_)
import           Common.Banner        ( banner )
import           Common.Eithers       ( hasData )
import           Common.Pages         ( Page(..) )
import           Common.Structure     ( renderStructure )

data Action
  = GetMagicItems
  | DecodeMagicItems (Response MisoString)
  | SetMagicItems (Either MisoString [MagicItem])
  | PostMagicItems
  | ErrorHandler (Response MisoString)
  | UpdateFilter MagicItemFilter

magicItemsTopic :: Topic MagicItemsModel
magicItemsTopic = topic "magicItems"

updateModel :: Action -> Effect a props MagicItemsModel Action
updateModel GetMagicItems         = getText "./data/magicitems.json" [] DecodeMagicItems ErrorHandler
updateModel (DecodeMagicItems r)  = issue $ SetMagicItems (eitherDecode (body r))
updateModel (SetMagicItems r)     = magicItems .= r >> issue PostMagicItems
updateModel PostMagicItems        = get >>= (io_ . publish magicItemsTopic)
updateModel (ErrorHandler r)      = maybe (pure ()) mailParent (errorMessage r)
updateModel (UpdateFilter f)      = itemFilter .= f >> issue PostMagicItems

viewModel :: props -> MagicItemsModel -> View MagicItemsModel Action
viewModel _ m =
  H.div_ [ P.class_ "h-screen flex flex-col"]
  [ banner MagicItems
    , filterView m
    , H.div_ [ P.class_ "overflow-y-auto flex-1" ]
      ( case (m ^. magicItems) of
          Right _ -> (map magicItemView (filteredMagicItems m))
          Left  e -> [ text $ "Error: [" <> e <> "]" ]
      )
  ]

filterView :: MagicItemsModel -> View MagicItemsModel Action
filterView m =
  H.div_ [ P.class_ "sticky top-0 z-10 bg-white border-b gap-3 p-4", MC.style_ [ MC.width "100%" ] ]
  [ H.table_ [ MC.style_ [ MC.width "100%" ] ]
    [ H.tr_ [MC.style_ [ MC.width "100%" ] ]
      [ H.td_ [ MC.style_ [ MC.width "33%" ] ]
        [ H.label_ [ P.class_ "label", P.for_ "itemFilterTitle" ] [ "Name" ]
        , H.input_ [ P.placeholder_ "Title",  P.class_ "input", P.type_ "text", P.id_ "itemFilterTitle", P.value_ ((m ^. itemFilter) ^. flt_title),  E.onInput (\s -> UpdateFilter ((m ^. itemFilter) { _flt_title = s })) ]
        ]
      , H.td_ [ MC.style_ [ MC.width "33%" ] ]
        [ H.label_ [ P.class_ "label", P.for_ "itemFilterRarity" ] [ "Level" ]
          , H.select_ [ MC.style_ [ MC.width "100%"], P.placeholder_ "Item Rarity", P.class_ "select", P.id_ "itemFilterRarity" , E.onInput (\s -> UpdateFilter ((m ^. itemFilter) { _flt_rarity = strToRarity s })) ]
          ( (mkOption "All Rarities" "") : map (\s -> mkOption s s) allRarities )
        ]
      , H.td_ [ MC.style_ [ MC.width "33%" ] ]
        [ H.label_ [ P.class_ "label", P.for_ "itemFilterType" ] [ "Type" ]
          , H.select_ [ MC.style_ [ MC.width "100%"], P.placeholder_ "Item Type", P.class_ "select", P.id_ "itemFilterType" , E.onInput (\s -> UpdateFilter ((m ^. itemFilter) { _flt_type = strToItemType s })) ]
          ( (mkOption "All Types" "") : map (\s -> mkOption s s) allItemTypes )
        ]
      ]
    ]
  ]

strToItemType :: MisoString -> Maybe ItemType
strToItemType "Armour" = Just $ ItemTypeArmour Nothing
strToItemType "Item" = Just $ ItemTypeItem Nothing
strToItemType "Potion" = Just $ ItemTypePotion Nothing
strToItemType "Ring" = Just $ ItemTypeRing Nothing
strToItemType "Rod" = Just $ ItemTypeRod Nothing
strToItemType "Scroll" = Just $ ItemTypeScroll Nothing
strToItemType "Shield" = Just $ ItemTypeShield Nothing
strToItemType "Staff" = Just $ ItemTypeStaff Nothing
strToItemType "Wand" = Just $ ItemTypeWand Nothing
strToItemType "Weapon" = Just $ ItemTypeWeapon Nothing
strToItemType _ = Nothing

strToRarity :: MisoString -> Maybe Rarity
strToRarity "Common"    = Just $ RarityCommon
strToRarity "Uncommon"  = Just $ RarityUncommon
strToRarity "Rare"      = Just $ RarityRare
strToRarity "Very Rare" = Just $ RarityVeryRare
strToRarity "Legendary" = Just $ RarityLegendary
strToRarity "Artifact"  = Just $ RarityArtifact
strToRarity "Unique"    = Just $ RarityUnique
strToRarity "Unknown"   = Just $ RarityUnknown "Unknown"
strToRarity _           = Nothing

allItemTypes :: [MisoString]
allItemTypes = map (ms . show) ([minBound .. maxBound] :: [ItemType])

allRarities :: [MisoString]
allRarities = map (ms . show) ([minBound .. maxBound] :: [Rarity])

mkOption :: MisoString -> MisoString -> View MagicItemsModel Action
mkOption caption value = H.option_ [ P.value_ value ] [ text caption ]

filteredMagicItems :: MagicItemsModel -> [MagicItem]
filteredMagicItems m =
  case (m ^. magicItems) of
    (Left _  ) -> []
    (Right xs) -> filter (filterMagicItem (m ^. itemFilter)) xs

filterMagicItem :: MagicItemFilter -> MagicItem -> Bool
filterMagicItem f m = filterTitle (f ^. flt_title) (_title m) && filterRarity (f ^. flt_rarity) (_rarity m) && filterItemType (f ^. flt_type) (_itemtype m)

filterTitle :: MisoString -> MisoString -> Bool
filterTitle "" _ = True
filterTitle f  t = L.isInfixOf (map C.toLower (fromMisoString f)) (map C.toLower (fromMisoString t))

filterRarity :: Maybe Rarity -> [Rarity] -> Bool
filterRarity Nothing  _ = True
filterRarity (Just f) r = L.elem f r

filterItemType :: Maybe ItemType -> ItemType -> Bool
filterItemType Nothing  _ = True
filterItemType (Just f) t = f == t

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
    [ H.td_ [ MC.style_ [ MC.width "33%" ] ] [ H.a_ [ P.href_ (_url m)] [ text (ms $ _title m) ] ]
    , H.td_ [ MC.style_ [ MC.width "33%" ] ] [ text (ms $ L.intercalate ", " $ map show (_rarity m)) ]
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
bodyView m = map renderStructure (_description m) <>
  [ H.div_ [] [text (ms $ prettyAttunement $ _attunement m)]
    , H.div_ [] [text (ms $ "Source: " <> intercalate ", " (map prettySource (_source m)))]
  ]

prettyAttunement :: Attunement -> MisoString
prettyAttunement AttuneNone        = "Does not require attunement"
prettyAttunement (Attune Nothing)  = "Requires attunement"
prettyAttunement (Attune (Just s)) = "Requires attunement by a " <> s

prettySource :: SourceBook -> MisoString
prettySource SourceAcquisitionsIncorporated = "Acquisitions Incorporated"
prettySource SourceBaldursGateDescentIntoAvernus = "Baldurs Gate Descent Into Avernus"
prettySource SourceBigbyPresentsGloryOfTheGiants = "Bigby Presents Glory Of The Giants"
prettySource SourceBookOfManyThings = "Book Of Many Things"
prettySource SourceCandlekeepMysteries = "Candlekeep Mysteries"
prettySource SourceCriticalRoleCallOfNetherdeep = "Critical Role Call Of Netherdeep"
prettySource SourceCurseOfStrahd = "Curse Of Strahd"
prettySource SourceDivineContention = "Divine Contention"
prettySource SourceDMG = "Dungeon Masters Guide"
prettySource SourceDragonlanceShadowOfTheDragonQueen = "Dragonlance Shadow Of The Dragon Queen"
prettySource SourceDungeonsAndDragonsHonorAmongThieves = "Dungeons And Dragons Honor Among Thieves"
prettySource SourceEberronRisingFromTheLastWar = "Eberron Rising From The Last War"
prettySource SourceExplorersGuideToWildemount = "Explorers Guide To Wildemount"
prettySource SourceFizbansTreasuryOfDragons = "Fizbans Treasury Of Dragons"
prettySource SourceGhostsOfSaltmarsh = "Ghosts Of Saltmarsh"
prettySource SourceGuildmastersGuideToRavnica = "Guildmasters Guide To Ravnica"
prettySource SourceIcewindDaleRimeOfTheFrostmaiden = "Icewind Dale Rime Of The Frostmaiden"
prettySource SourceInfernalMachineRebuild = "Infernal Machine Rebuild"
prettySource SourceJourneysThroughTheRadiantCitadel = "Journeys Through The Radiant Citadel"
prettySource SourceKeysFromTheGoldenVault = "Keys From The Golden Vault"
prettySource SourceLostLaboratoryOfKwalish = "Lost Laboratory Of Kwalish"
prettySource SourceLostMineOfPhandelver = "Lost Mine Of Phandelver"
prettySource SourceMonstrousCompendium2 = "Monstrous Compendium 2"
prettySource SourceMythicOdysseysOfTheros = "Mythic Odysseys Of Theros"
prettySource SourceOutOfTheAbyss = "Out Of The Abyss"
prettySource SourcePhandelverAndBelowTheShatteredObelisk = "Phandelver And Below The Shattered Obelisk"
prettySource SourcePlanescapeAdventuresInTheMultiverse = "Planescape Adventures In The Multiverse"
prettySource SourcePrincesOfTheApocalypse = "Princes Of The Apocalypse"
prettySource SourceQuestsFromTheInfiniteStaircase = "Quests From The Infinite Staircase"
prettySource SourceSleepingDragonsWake = "Sleeping Dragons Wake"
prettySource SourceSpelljammerAdventuresInSpace = "Spelljammer Adventures In Space"
prettySource SourceStormKingsThunder = "Storm Kings Thunder"
prettySource SourceStrixhavenCurriculumOfChaos = "Strixhaven Curriculum Of Chaos"
prettySource SourceTalesFromTheYawningPortal = "Tales From The Yawning Portal"
prettySource SourceTashasCauldronOfEverything = "Tashas Cauldron Of Everything"
prettySource SourceTheRiseOfTiamat = "The Rise Of Tiamat"
prettySource SourceTheWildBeyondTheWitchlight = "The Wild Beyond The Witchlight"
prettySource SourceTombOfAnnihilation = "Tomb Of Annihilation"
prettySource SourceTyrannyOfDragons = "Tyranny Of Dragons"
prettySource SourceVanRichtensGuideToRavenloft = "Van Richtens Guide To Ravenloft"
prettySource SourceVecnaEyeOfRuin = "Vecna Eye Of Ruin"
prettySource SourceVolosGuideToMonsters = "Volos Guide To Monsters"
prettySource SourceWaterdeepDragonHeist = "Waterdeep Dragon Heist"
prettySource SourceWaterdeepDungeonOfTheMadMage = "Waterdeep Dungeon Of The Mad Mage"
prettySource SourceWayfarersGuideToEberron = "Wayfarers Guide To Eberron"
prettySource SourceXanatharsGuideToEverything = "Xanathars Guide To Everything"
prettySource (SourceUnknown s) = s

magicItemsComponent :: MagicItemsModel -> Component parent props MagicItemsModel Action
magicItemsComponent x = (vcomp x updateModel viewModel) { mount = if ( hasData $ _magicItems x ) then Nothing else Just GetMagicItems }
