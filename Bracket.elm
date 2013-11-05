module Bracket where
{-
  Bracket is a module for representing tournament "Brackets"
-}

import Maybe
import Color

type Brack = { score: Int,
               name:  String }

data Match = Empty
           | One Brack
           | Two Brack Brack

-- Any over matches
anyMatch : (Brack -> Bool) -> Match -> Bool
anyMatch fun m = case m of
                  Empty -> False
                  One b -> fun b
                  Two b1 b2 -> fun b1 || fun b2

-- Ctors for common types
player name = Brack 0 name
playerWithScore name score = Brack score name

-- Two brackets are deemed equal if their type and names are equal
brackEq b1 b2 = .name b1 == .name b2

matchEq : Match -> Match -> Bool
matchEq m1 m2 = case (m1,m2) of
                  (Empty,Empty) -> True
                  (One b1,One b2) -> brackEq b1 b2
                  (Two b1 b2,Two b3 b4) -> brackEq b1 b3 && brackEq b2 b4
                  _ -> False

maxScore : Match -> Maybe Brack
maxScore m = case m of
              Empty -> Nothing
              One b -> Just b
              Two b1 b2 -> if | .score b1 > .score b2 -> Just b1
                              | .score b2 > .score b1 -> Just b2
                              | otherwise -> Nothing
-- Tree bracket structure
data Bracket = Leaf Match
             | InnerNode Match Bracket Bracket

match : Bracket -> Match
match b = case b of {Leaf m -> m; InnerNode m _ _ -> m}

finished : Bracket -> Bool
finished b = anyBracket
              (\m -> maxScore m
                     |> maybe False
                         (\b -> if .score b >= 2
                                then True
                                else False))
              b

mapBracket : (Match -> Match) -> Bracket -> Bracket
mapBracket fun b = case b of
  Leaf a -> Leaf (fun a)
  InnerNode a b1 b2 -> InnerNode (fun a) (mapBracket fun b1) (mapBracket fun b2)

anyBracket : (Match -> Bool) -> Bracket -> Bool
anyBracket fun b =
  case b of
     Leaf a -> (fun a)
     InnerNode a b1 b2 -> (fun a) || anyBracket fun b1 || anyBracket fun b2

-- Build an initial bracket from a list of names
fromList : [String] -> Bracket
fromList players =
  let minDepth : Bracket -> Int
      minDepth b = case b of
          Leaf _ -> 0
          InnerNode _ b1 b2 -> 1 + min (minDepth b1) (minDepth b2)
      hasFreeSpot : Bracket -> Bool
      hasFreeSpot b = case b of
          Leaf m -> case m of
                      Empty -> True
                      One _ -> True
                      _ -> False
          InnerNode m b1 b2 -> hasFreeSpot b1 || hasFreeSpot b2
      chooseSide : String -> Bracket -> Bracket
      chooseSide p b =
        case b of
          Leaf m -> Leaf m
          InnerNode m b1 b2 ->
                          let b1dep = minDepth b1
                              b2dep = minDepth b2
                          in
                          if b1dep == b2dep then
                                InnerNode Empty (Leaf (One (player p))) b
                          else if b1dep > b2dep then
                                InnerNode Empty b1 (consBracket p b2)
                          else
                                InnerNode Empty (consBracket p b1) b2
      consBracket : String -> Bracket -> Bracket
      consBracket p b =
        case b of
          Leaf m -> case m of
                      Empty -> Leaf (One (player p))
                      One p1 -> Leaf (Two p1 (player p))
                      Two _ _ -> InnerNode Empty (Leaf (One (player p))) b
          InnerNode m b1 b2 ->
                    if | hasFreeSpot b1 -> InnerNode m (consBracket p b1) b2
                       | hasFreeSpot b2 -> InnerNode m b1 (consBracket p b2)
                       | otherwise -> chooseSide p b
      build : [String] -> Bracket -> Bracket
      build ps b =
        case ps of
          p1 :: [] -> consBracket p1 b
          p1 :: ps' -> build ps' (consBracket p1 b)
  in build players (Leaf Empty)
