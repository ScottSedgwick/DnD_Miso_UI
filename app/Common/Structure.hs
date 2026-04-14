{-# LANGUAGE OverloadedStrings #-}
module Common.Structure where

import           Control.Applicative ( empty )
import           Data.Map            ( fromList, member, singleton )
import           Miso                ( MisoString, View, ms, text )
import qualified Miso.Html           as H
import qualified Miso.Html.Property  as P
import           Miso.JSON           ( FromJSON, Value(..), ToJSON, (.:), parseJSON, toJSON )

data Inline
  = T MisoString
  | B MisoString
  | I MisoString
  | A MisoString MisoString
  | H1 MisoString
  | H2 MisoString
  | H3 MisoString
  | H4 MisoString
  | H5 MisoString
  | H6 MisoString
  | BR
  deriving (Show, Eq)

instance FromJSON Inline where
  parseJSON (Object v)
    | member "t"  v = T <$> v .: "t"
    | member "b"  v = B <$> v .: "b"
    | member "i"  v = I <$> v .: "i"
    | member "a"  v = A <$> v .: "a" <*> v .: "c"
    | member "h1" v = H1 <$> v .: "h1"
    | member "h2" v = H2 <$> v .: "h2"
    | member "h3" v = H3 <$> v .: "h3"
    | member "h4" v = H4 <$> v .: "h4"
    | member "h5" v = H5 <$> v .: "h5"
  parseJSON _ = empty

instance ToJSON Inline where
  toJSON (T s)   = Object (singleton "t" (String s))
  toJSON (B s)   = Object (singleton "b" (String s))
  toJSON (I s)   = Object (singleton "i" (String s))
  toJSON (A u s) = Object (fromList [("a", (String u)),("c", (String s))])
  toJSON (H1 s)  = Object (singleton "h1" (String s))
  toJSON (H2 s)  = Object (singleton "h2" (String s))
  toJSON (H3 s)  = Object (singleton "h3" (String s))
  toJSON (H4 s)  = Object (singleton "h4" (String s))
  toJSON (H5 s)  = Object (singleton "h5" (String s))
  toJSON (H6 s)  = Object (singleton "h6" (String s))
  toJSON BR      = Object (singleton "br" (String ""))

data Structure
  = P [Inline]
  | UL [[Inline]]
  | TB [[[Inline]]]
  | RT [[Inline]]
  | ST [[Inline]]
  | ST1 [[Inline]]
  deriving (Show, Eq)

instance FromJSON Structure where
  parseJSON (Object v)
    | member "p"  v = P <$> v .: "p"
    | member "ul" v = UL <$> v .: "ul"
    | member "tb" v = TB <$> v .: "tb"
    | member "rt" v = RT <$> v .: "rt"
    | member "st" v = ST <$> v .: "st"
    | member "st1" v = ST1 <$> v .: "st1"
    | otherwise = empty
  parseJSON _ = empty

instance ToJSON Structure where
  toJSON (P  xs) = Object (singleton "p"  (toJSON xs))
  toJSON (UL xs) = Object (singleton "ul" (toJSON xs))
  toJSON (TB xs) = Object (singleton "tb" (toJSON xs))
  toJSON (RT xs) = Object (singleton "rt" (toJSON xs))
  toJSON (ST xs) = Object (singleton "st" (toJSON xs))
  toJSON (ST1 xs) = Object (singleton "st1" (toJSON xs))

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x:_) = Just x
firstJust (_:xs)     = firstJust xs
firstJust []         = Nothing

renderStructure :: Structure -> View m a
renderStructure (P xs) = H.p_ [] (map renderInline xs)
renderStructure (UL xs) = H.ul_ [] (map (\x -> H.li_ [] (map renderInline x)) xs)
renderStructure (TB xs) = table xs
renderStructure (RT xs) = rollTable "Description" xs
renderStructure (ST xs) = spellTable "Spell" xs
renderStructure (ST1 xs) = spellTable1 "Spell" xs

renderInline :: Inline -> View m a
renderInline (T s)     = text s
renderInline (B s)     = H.b_ [] [ text s ]
renderInline (I s)     = H.i_ [] [ text s ]
renderInline (A s1 s2) = H.a_ [ P.href_ s1 ] [ text s2 ]
renderInline (H1 s)    = H.h1_ [] [ text s ]
renderInline (H2 s)    = H.h2_ [] [ text s ]
renderInline (H3 s)    = H.h3_ [] [ text s ]
renderInline (H4 s)    = H.h4_ [] [ text s ]
renderInline (H5 s)    = H.h5_ [] [ text s ]
renderInline (H6 s)    = H.h6_ [] [ text s ]
renderInline BR        = H.br_ []
-- renderInline (Plain s)        = H.p_ [] [ text (ms s) ]
-- renderInline (Bold s)         = H.p_ [] [ H.b_ [] [ text (ms s) ] ]
-- renderInline (Italic s)       = H.p_ [] [ H.em_ [] [ text (ms s) ] ]
-- renderInline (UList xs)       = H.ul_ [] (map (\s -> H.li_ [] [text (ms s)]) xs)
-- renderInline (RollTable xs)   = rollTable "Description" xs
-- renderInline (SpellTable xs)  = spellTable "Spells" xs
-- renderInline (SpellTable1 xs) = spellTable1 "Spells" xs
-- renderInline (Table xs)       = table xs

zipEm :: [a] -> [a] -> [[a]]
zipEm [] _ = []
zipEm _ [] = []
zipEm (x:xs) (y:ys) = [x,y] : zipEm xs ys

spellLevels :: [[Inline]]
spellLevels = [[T "Cantrip"], [T "1st"], [T "2nd"], [T "3rd"], [T "4th"], [T "5th"], [T "6th"], [T "7th"], [T "8th"], [T "9th"]]

rollTable :: MisoString -> [[Inline]] -> View m a
rollTable title xs = table $ zipEm col1 col2
  where
    col1 = [T $ ms $ "d" <> show (length xs)] : map (\n -> [T $ ms $ show n]) ([1..] :: [Int])
    col2 = [T title] : xs

spellTable :: MisoString -> [[Inline]] -> View m a
spellTable title xs = table $ zipEm col1 col2
  where
    col1 = [T "Spell Level"] : spellLevels
    col2 = [T title] : xs

spellTable1 :: MisoString -> [[Inline]] -> View m a
spellTable1 title xs = table $ zipEm col1 col2
  where
    col1 = [T "Spell Level"] : (drop 1 spellLevels)
    col2 = [T title] : xs

stripeTable :: [[Inline]] -> [[[Inline]]] -> View m a
stripeTable ts xs =
  H.table_ [ P.class_ "stripes" ]
  [ H.thead_ []
    [ H.tr_ [] (map (\t -> H.th_ [] (map renderInline t)) ts)
    ]
  , H.tbody_ [] (map mkStripeRow xs)
  ]

mkStripeRow :: [[Inline]] -> View m a
mkStripeRow xs = 
  H.tr_ [] (map (\x -> H.td_ [] (map renderInline x)) xs)

table :: [[[Inline]]] -> View m a
-- table _ = H.p_ [] [ text "table goes here" ]
table [] = stripeTable [] []
table (x:xs) = stripeTable x xs