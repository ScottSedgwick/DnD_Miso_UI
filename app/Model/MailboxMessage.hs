{-# LANGUAGE OverloadedStrings #-}
module Model.MailboxMessage where

import Miso                  ( MisoString )
import Miso.PubSub           ( Topic, topic )
import Model.BackgroundModel ( Background )
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