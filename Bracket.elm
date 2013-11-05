module Bracket where
{-
  Bracket is a module for representing tournament "Brackets"
-}

import Maybe
import Color

type Brack = { score:    Int,
               name:     String,
               selected: Bool }

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
player name = Brack 0 name False
playerWithScore name score = Brack score name False

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

gayPurple = solid (rgb 79 54 153)
lineStyle = { gayPurple | width <- 3,
                          cap <- Round,
                          join <- Sharp 0.1}


renderBracket : Bracket -> (Match -> (Int, Int, Form)) -> (Int, Int, [Form])
renderBracket b draw =
  case b of
    Leaf match ->
      let (w,h,drawn) = draw match in
      (w,h,[drawn])
    InnerNode match bl br ->
      let (w1,h1,left)  = renderBracket bl draw
          (w2,h2,right) = renderBracket br draw
          (w,h, drawn)  = draw match
          left'         = map (move (toFloat (-w - 50), toFloat -h1)) left
          right'        = map (move (toFloat (-w - 50), toFloat h2))  right
          tarrw         = traced lineStyle
                           (path [(toFloat -w/2 - 50, toFloat -h1),
                                  (toFloat -w/2 - 25, toFloat -h1),
                                  (toFloat -w/2 - 25,0)])
          barrw         = traced lineStyle
                           (path [(toFloat -w/2 - 50, toFloat h2),
                                  (toFloat -w/2  - 25, toFloat h2),
                                  (toFloat -w/2  - 25,0),
                                  (toFloat -w/2, 0)])
      in
      (w+w1,
       h1+h2,
       drawn :: tarrw :: barrw :: (left' ++ right'))
