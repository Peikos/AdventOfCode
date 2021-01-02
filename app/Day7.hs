{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveFunctor, TypeFamilies #-}

module Day7 where

import Util
import Parser

import Control.Applicative
import Data.Bifunctor
import Data.Fix
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Colour = String
data TreeF v a = Node { val :: v, children :: [a] } deriving (Show, Eq, Functor)

readData :: String -> Map Colour [(Int, Colour)]
readData = Map.fromList . catMaybes . map (parse parseBagRule) . lines

pColour :: Parser String
pColour = pOneOf colours

parseHasBags :: Parser (Int, Colour)
parseHasBags = do i <- pInteger
                  _ <- pWhitespace
                  c <- pColour
                  _ <- pWhitespace
                  _ <- pString "bags" <|> pString "bag"
                  return (i, c)

parseEmptyBag :: Parser [(Int, Colour)]
parseEmptyBag = const [] <$> pString "no other bags"

parseBagRule :: Parser (Colour, [(Int, Colour)])
parseBagRule = do c <- pColour
                  _ <- pString " bags contain "
                  rs <- parseEmptyBag <|> pRepeatSepBy (trimmed $ pChar ',') parseHasBags
                  _ <- pChar '.'
                  return (c, rs)

containedInCoalgebra ::  Map Colour [(Int, Colour)] -> Coalgebra (TreeF Colour) Colour
containedInCoalgebra kb c = Node c . map fst . Map.toList . Map.filter (\v -> c `elem` map snd v) $ kb

containedInAlgebra :: Algebra (TreeF Colour) (Set Colour)
containedInAlgebra (Node c cs) = Set.insert c (foldr Set.union Set.empty cs)

mustContainCoalgebra ::  Map Colour [(Int, Colour)] -> Coalgebra (TreeF (Int, Colour)) (Int, Colour)
mustContainCoalgebra kb (i, c) = Node (i, c) . fromMaybe [] $ Map.lookup c $ kb

mustContainAlgebra :: Algebra (TreeF (Int, Colour)) Int
mustContainAlgebra (Node (i, _) bs) = i + i * (sum bs)

main = do kb <- readData <$> readFile "data/input7"
          print $ pred $ length $ hylo containedInAlgebra (containedInCoalgebra kb) "shiny gold"
          print $ pred $ hylo mustContainAlgebra (mustContainCoalgebra kb) (1, "shiny gold")

colours = ["drab tan", "vibrant lime", "pale lime", "dull gray", "light fuchsia", "drab gold", "dim red", "striped orange", "shiny violet", "faded olive", "dotted lime", "drab magenta", "dull black", "posh turquoise", "muted purple", "striped black", "drab brown", "pale purple", "dark turquoise", "clear lavender", "bright coral", "posh coral", "drab lime", "striped brown", "wavy bronze", "dim black", "posh yellow", "pale violet", "mirrored bronze", "vibrant silver", "dark brown", "wavy green", "light beige", "wavy crimson", "clear tan", "posh lime", "pale blue", "wavy silver", "striped beige", "dim gold", "vibrant teal", "light tan", "dark red", "posh salmon", "mirrored yellow", "dark fuchsia", "bright bronze", "posh silver", "dotted salmon", "dark orange", "bright silver", "posh orange", "mirrored olive", "drab beige", "muted bronze", "dark lime", "dark olive", "dull white", "shiny gray", "dotted orange", "clear olive", "drab bronze", "drab chartreuse", "faded cyan", "plaid fuchsia", "light gold", "pale bronze", "shiny coral", "vibrant green", "vibrant violet", "muted fuchsia", "bright olive", "mirrored maroon", "pale salmon", "faded brown", "dark lavender", "plaid coral", "drab black", "shiny black", "muted plum", "shiny lavender", "mirrored blue", "dotted aqua", "light lavender", "bright orange", "muted tomato", "dim indigo", "faded turquoise", "wavy teal", "dotted bronze", "posh purple", "dull violet", "drab indigo", "mirrored crimson", "mirrored tan", "wavy fuchsia", "striped yellow", "striped white", "light turquoise", "clear blue", "faded blue", "dim aqua", "shiny magenta", "wavy maroon", "plaid teal", "bright tomato", "mirrored lavender", "vibrant purple", "posh aqua", "faded fuchsia", "dim silver", "bright brown", "dim olive", "striped teal", "posh plum", "faded teal", "striped indigo", "drab blue", "light olive", "vibrant red", "faded salmon", "shiny brown", "posh olive", "clear green", "mirrored violet", "light brown", "plaid blue", "dim green", "mirrored plum", "dotted turquoise", "dull salmon", "wavy plum", "bright blue", "posh tomato", "dotted lavender", "posh magenta", "faded beige", "vibrant plum", "pale green", "dull fuchsia", "light plum", "dotted olive", "striped gold", "bright violet", "dim yellow", "bright indigo", "plaid lime", "striped aqua", "dull maroon", "muted tan", "plaid tan", "dull beige", "posh cyan", "drab red", "drab gray", "muted red", "drab green", "dim gray", "dark white", "posh black", "dim turquoise", "drab lavender", "striped crimson", "vibrant olive", "dim maroon", "muted lime", "drab purple", "muted beige", "dotted green", "bright black", "wavy yellow", "faded yellow", "dim purple", "bright chartreuse", "vibrant maroon", "pale fuchsia", "dotted teal", "dull lime", "bright salmon", "bright magenta", "striped red", "pale teal", "clear cyan", "muted gray", "bright gold", "clear red", "striped purple", "plaid violet", "shiny crimson", "posh gold", "dim crimson", "posh red", "drab turquoise", "vibrant cyan", "light bronze", "dotted cyan", "drab fuchsia", "clear maroon", "dim salmon", "wavy aqua", "pale beige", "dark maroon", "light aqua", "dim tan", "vibrant brown", "light magenta", "clear turquoise", "dull crimson", "dull orange", "wavy black", "plaid olive", "plaid indigo", "dim brown", "muted indigo", "dull plum", "vibrant tan", "plaid tomato", "shiny white", "plaid red", "drab orange", "striped green", "shiny indigo", "drab yellow", "faded magenta", "muted yellow", "posh brown", "pale tomato", "dark aqua", "muted blue", "light silver", "muted coral", "muted green", "muted brown", "dim violet", "clear fuchsia", "muted violet", "posh violet", "dark violet", "dotted fuchsia", "vibrant indigo", "vibrant gray", "dotted plum", "vibrant magenta", "drab maroon", "dotted gold", "striped cyan", "pale silver", "drab coral", "vibrant chartreuse", "pale maroon", "wavy blue", "mirrored green", "shiny purple", "dim white", "posh maroon", "posh chartreuse", "dim orange", "clear gray", "dull purple", "muted olive", "pale lavender", "shiny blue", "plaid salmon", "mirrored silver", "light blue", "light orange", "faded tomato", "shiny lime", "shiny fuchsia", "light indigo", "faded aqua", "posh blue", "dotted crimson", "striped salmon", "clear brown", "clear orange", "shiny teal", "dim bronze", "shiny silver", "dim tomato", "vibrant white", "dark blue", "plaid gray", "mirrored indigo", "mirrored cyan", "dotted red", "clear crimson", "mirrored brown", "pale gray", "bright gray", "dotted purple", "dotted indigo", "wavy orange", "dark plum", "vibrant yellow", "vibrant bronze", "striped silver", "dull yellow", "wavy turquoise", "drab salmon", "posh green", "muted gold", "mirrored aqua", "wavy tan", "mirrored magenta", "mirrored lime", "dark crimson", "striped blue", "wavy chartreuse", "clear magenta", "dim beige", "striped tan", "faded green", "drab teal", "posh lavender", "bright tan", "light red", "light white", "faded gray", "light gray", "light teal", "muted teal", "dull chartreuse", "vibrant salmon", "mirrored gold", "dark yellow", "plaid crimson", "shiny gold", "pale olive", "dotted magenta", "striped violet", "faded lime", "muted magenta", "dim lime", "plaid cyan", "dotted maroon", "striped gray", "light violet", "vibrant gold", "vibrant lavender", "clear yellow", "muted black", "pale brown", "plaid purple", "vibrant fuchsia", "light purple", "mirrored white", "dull silver", "dark magenta", "faded red", "mirrored beige", "faded coral", "shiny cyan", "posh gray", "dim cyan", "dotted tomato", "dark teal", "muted cyan", "dotted yellow", "dark cyan", "dark bronze", "drab cyan", "drab white", "bright cyan", "dull lavender", "shiny olive", "posh fuchsia", "plaid aqua", "shiny plum", "striped chartreuse", "pale red", "plaid maroon", "shiny beige", "plaid silver", "light cyan", "faded silver", "light green", "faded maroon", "posh tan", "muted silver", "mirrored turquoise", "clear coral", "clear teal", "muted lavender", "shiny tan", "dark salmon", "plaid orange", "clear black", "bright red", "striped tomato", "light maroon", "dark indigo", "faded violet", "clear violet", "dotted violet", "dotted brown", "wavy gray", "faded black", "shiny bronze", "faded purple", "dim plum", "plaid turquoise", "clear tomato", "bright green", "drab tomato", "clear indigo", "bright crimson", "dark coral", "light salmon", "dark purple", "dotted beige", "muted crimson", "wavy gold", "faded crimson", "shiny chartreuse", "dull magenta", "shiny turquoise", "drab aqua", "dim teal", "light yellow", "striped olive", "dark green", "light black", "dull brown", "shiny green", "shiny salmon", "clear aqua", "wavy beige", "plaid yellow", "clear beige", "clear lime", "vibrant beige", "bright yellow", "plaid plum", "plaid green", "dull tan", "clear bronze", "vibrant crimson", "vibrant orange", "plaid beige", "dull indigo", "muted white", "shiny aqua", "bright fuchsia", "dull bronze", "muted maroon", "striped fuchsia", "shiny tomato", "wavy lime", "wavy tomato", "striped coral", "clear silver", "vibrant tomato", "faded gold", "striped bronze", "dotted chartreuse", "vibrant coral", "faded lavender", "dull turquoise", "shiny yellow", "plaid lavender", "mirrored fuchsia", "wavy white", "clear purple", "dark chartreuse", "wavy purple", "bright lavender", "shiny red", "pale aqua", "wavy brown", "clear chartreuse", "mirrored chartreuse", "pale turquoise", "dotted blue", "bright plum", "pale black", "dotted coral", "plaid magenta", "striped lavender", "striped magenta", "pale tan", "light crimson", "pale coral", "vibrant blue", "dark tan", "drab olive", "wavy violet", "plaid gold", "plaid bronze", "wavy cyan", "vibrant black", "faded white", "mirrored orange", "dark gold", "muted turquoise", "dull blue", "light tomato", "striped turquoise", "wavy magenta", "posh bronze", "drab silver", "bright maroon", "dull red", "pale gold", "wavy red", "mirrored gray", "drab plum", "mirrored teal", "dotted tan", "dark gray", "pale indigo", "bright white", "clear white", "faded tan", "striped lime", "dim blue", "mirrored tomato", "wavy coral", "dark silver", "dim fuchsia", "light lime", "mirrored red", "light chartreuse", "dull teal", "dull aqua", "pale white", "wavy salmon", "faded chartreuse", "posh beige", "posh crimson", "shiny orange", "wavy lavender", "vibrant aqua", "dim magenta", "wavy olive", "dull green", "dotted gray", "mirrored salmon", "faded orange", "plaid black", "dull coral", "pale chartreuse", "mirrored black", "drab crimson", "muted orange", "pale orange", "clear salmon", "dull tomato", "pale crimson", "dotted white", "posh teal", "dim coral", "bright aqua", "dark beige", "bright teal", "dotted silver", "striped maroon", "pale magenta", "shiny maroon", "dark black", "posh white", "muted aqua", "pale cyan", "plaid white", "bright beige", "dotted black", "bright purple", "faded plum", "dull gold", "dark tomato", "pale yellow", "vibrant turquoise", "mirrored coral", "muted salmon", "pale plum", "clear gold", "muted chartreuse", "light coral", "faded indigo", "bright turquoise", "clear plum", "plaid chartreuse", "faded bronze", "bright lime", "drab violet", "dull olive", "wavy indigo", "posh indigo", "dull cyan", "plaid brown", "mirrored purple", "striped plum", "dim lavender", "dim chartreuse"]

