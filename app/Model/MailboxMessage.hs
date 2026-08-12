module Model.MailboxMessage where

import Miso                  ( MisoString )
import Miso.PubSub           ( Topic, topic )
import Model.BackgroundModel ( Background )
import Model.FeatsModel      ( Feat )
import Model.PoisonModel     ( Poison )
import Model.SpellsModel     ( Spell, SpellFilter )

counterTopic :: Topic Integer
counterTopic = topic "counterState"

backgroundsTopic :: Topic [Background]
backgroundsTopic = topic "backgrounds"

backgroundFilterTopic :: Topic MisoString
backgroundFilterTopic = topic "backgroundFilter"

spellsTopic :: Topic [Spell]
spellsTopic = topic "spells"

spellFilterTopic :: Topic SpellFilter
spellFilterTopic = topic "spellFilter"

insultsTopic :: Topic [MisoString]
insultsTopic = topic "insults"

currentInsultTopic :: Topic MisoString
currentInsultTopic = topic "currentInsult"

poisonsTopic :: Topic [Poison]
poisonsTopic = topic "poisons"

poisonFilterTopic :: Topic MisoString
poisonFilterTopic = topic "poisonFilter"

featsFilterTopic :: Topic MisoString
featsFilterTopic = topic "featsFilterTopic"

featsTopic :: Topic [Feat]
featsTopic = topic "featsTopic"
