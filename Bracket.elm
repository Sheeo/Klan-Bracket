module Bracket where
{-
  Bracket is a module for representing tournament "Brackets"
-}

import Maybe
import Color

data ParentRelation = WinnerOf | LoserOf
type Brack = { score:  Maybe Int,
               name:   Maybe String,
               parent: Maybe (ParentRelation, String) }

type Match = { title:  String,
               top:    Brack,
               bottom: Brack }

data Bracket = Leaf Match
             | InnerNode Match Bracket Bracket

data EliminationStyle = Single | Double
data WinningCriteria = BestOf Int
type BracketStyle = { elimination: EliminationStyle,
                      winCriteria: WinningCriteria }


-- Any over matches
anyMatch : (Brack -> Bool) -> Match -> Bool
anyMatch fun match = fun (.top match) || fun (.bottom match)

-- Ctors for common types
player name = Brack (Just 0) (Just name) Nothing
playerWithScore name score = Brack (Just score) (Just name) Nothing
emptyPlayer = Brack Nothing Nothing Nothing
emptyMatch title = Match title emptyPlayer emptyPlayer

-- Two brackets are deemed equal if their names are equal
brackEq b1 b2 = .name b1 == .name b2

-- Matches are equalized by their title
matchEq : Match -> Match -> Bool
matchEq m1 m2 = .title m1 == .title m2

-- Pick the topmost match off of a bracket
topMatch : Bracket -> Match
topMatch b = case b of {Leaf m -> m; InnerNode m _ _ -> m}

leftMatch : Bracket -> Maybe Match
leftMatch b = case b of {Leaf m -> Nothing; InnerNode _ bl _ -> Just (topMatch bl)}

rightMatch : Bracket -> Maybe Match
rightMatch b = case b of {Leaf m -> Nothing; InnerNode _ _ br -> Just (topMatch br)}

bothJust : (Maybe a, Maybe b) -> Maybe (a,b)
bothJust (a,b) = case (a,b) of
                  (Just x, Just y) -> Just (x,y)
                  _ -> Nothing

matchFinished : WinningCriteria -> Match -> Bool
matchFinished w m =
  let topS = .score (.top m)
      botS = .score (.bottom m)
  in case w of
    BestOf n -> case (topS, botS) of
                  (Just a, Just b) -> if a + b >= n then True else False
                  _ -> False

bracketFinished : WinningCriteria -> Bracket -> Bool
bracketFinished w b = allBracket (matchFinished w) b

mapBracket : (Match -> Match) -> Bracket -> Bracket
mapBracket fun b = case b of
  Leaf a -> Leaf (fun a)
  InnerNode a b1 b2 -> InnerNode (fun a) (mapBracket fun b1) (mapBracket fun b2)

foldBracket : (Match -> a -> a) -> a -> Bracket -> a
foldBracket fun acc b =
  case b of
    Leaf a -> fun a acc
    InnerNode a top bot ->
      let topacc = foldBracketLeaf fun acc top
          botacc = foldBracketLeaf fun topacc bot
      in fun a botacc

foldBracketLeaf : (Match -> a -> a) -> a -> Bracket -> a
foldBracketLeaf fun acc b =
  case b of
    Leaf a -> fun a acc
    InnerNode _ top bot ->
      let topacc = foldBracketLeaf fun acc top
      in foldBracketLeaf fun topacc bot

anyBracketLeaf : (Match -> Bool) -> Bracket -> Bool
anyBracketLeaf fun b =
  case b of
    Leaf a -> fun a
    InnerNode _ top bot -> anyBracketLeaf fun top || anyBracketLeaf fun bot

anyBracket : (Match -> Bool) -> Bracket -> Bool
anyBracket fun b =
  case b of
    Leaf a -> (fun a)
    InnerNode a b1 b2 -> (fun a) || anyBracket fun b1 || anyBracket fun b2

allBracket : (Match -> Bool) -> Bracket -> Bool
allBracket fun b = anyBracket (\m -> not (fun m)) b

-- Build an initial bracket from a list of names
fromList : BracketStyle -> [String] -> Bracket
fromList bracketStyle players =
  let minDepth : Bracket -> Int
      minDepth b = case b of
          Leaf _ -> 0
          InnerNode _ b1 b2 -> 1 + min (minDepth b1) (minDepth b2)
      hasFreeSpot : Bracket -> Bool
      hasFreeSpot = anyBracketLeaf <| anyMatch <| isNothing . (.name)
      nextMatchId : Bracket -> Int
      nextMatchId b = foldBracketLeaf (\_ acc -> acc + 1) 1 b
      nilmatch = emptyMatch ""
      chooseSide : String -> Bracket -> Bracket -> Bracket
      chooseSide playerName b bfull =
        case b of
          Leaf m -> b
          InnerNode m top bot ->
                          let topdep = minDepth top
                              botdep = minDepth bot
                          in
                          if topdep == botdep then
                                InnerNode nilmatch b (Leaf (Match (nextMatchId bfull |> show) (player playerName) emptyPlayer))
                          else if topdep > botdep then
                                InnerNode nilmatch top (consBracket playerName bot bfull)
                          else
                                InnerNode nilmatch (consBracket playerName top bfull) bot
      consBracket : String -> Bracket -> Bracket -> Bracket
      consBracket p b bfull =
        case b of
          Leaf m -> if | .name (.top m) == Nothing    -> Leaf {m | top <- player p}
                       | .name (.bottom m) == Nothing -> Leaf {m | bottom <- player p}
                       | otherwise -> let top = Leaf m
                                          bot = Leaf (Match (nextMatchId bfull |> show) (player p) emptyPlayer)
                                      in
                                      InnerNode (Match ""
                                                       (Brack Nothing Nothing (Just (WinnerOf, .title (topMatch top))))
                                                       (Brack Nothing Nothing (Just (WinnerOf, .title (topMatch bot)))))
                                                top
                                                bot
          InnerNode m b1 b2 ->
                    if | hasFreeSpot b1 -> InnerNode m (consBracket p b1 bfull) b2
                       | hasFreeSpot b2 -> InnerNode m b1 (consBracket p b2 bfull)
                       | otherwise -> chooseSide p b bfull
      build : [String] -> Bracket -> Bracket
      build ps b =
        case ps of
          p1 :: [] -> consBracket p1 b b
          p1 :: ps' -> build ps' (consBracket p1 b b)
  in build players (Leaf (emptyMatch "1"))

-- Update a bracket with matches determined from winners
updateBracket : WinningCriteria -> Bracket -> Bracket
updateBracket criteria b =
  case b of
      Leaf m -> b
      InnerNode m b1 b2 ->
        let b1' = updateBracket criteria b1
            b2' = updateBracket criteria b2
        in b

-- Given a match, update it in the bracket
updateScore : Match -> Bracket -> Bracket
updateScore m b = mapBracket (\m' -> if matchEq m m' then m
                                     else m')
                              b
