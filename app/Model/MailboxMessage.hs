{-# LANGUAGE OverloadedStrings #-}
module Model.MailboxMessage where

import Miso                  ( MisoString )
import Miso.PubSub           ( Topic, topic )
import Model.BackgroundModel ( Background )
import Model.SpellsModel     ( Spell )

counterTopic :: Topic Integer
counterTopic = topic "counterState"

backgroundsTopic :: Topic [Background]
backgroundsTopic = topic "backgrounds"

backgroundFilterTopic :: Topic MisoString
backgroundFilterTopic = topic "backgroundFilter"

spellsTopic :: Topic [Spell]
spellsTopic = topic "backgrounds"

spellsFilterTopic :: Topic MisoString
spellsFilterTopic = topic "backgrounds"