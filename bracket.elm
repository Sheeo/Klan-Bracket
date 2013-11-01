module Bracket where

data BrackType = Player
               | Team

type Brack = { score:    Int,
               name:     String,
               selected: Bool,
               typ:      BrackType}

data Match = Empty
           | One Brack
           | Two Brack Brack

anyMatch : (Brack -> Bool) -> Match -> Bool
anyMatch fun m = case m of
                  Empty -> False
                  One b -> fun b
                  Two b1 b2 -> fun b1 || fun b2

player name = Brack 0 name False Player
team name   = Brack 0 name False Team
teamWithScore name score   = Brack score name False Team
playerWithScore name score = Brack score name False Player

-- Two brackets are deemed equal if their type and names are equal
brackEq b1 b2 = .name b1 == .name b2 && .typ b1 == .typ b2

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
data Bracket a = Leaf a
               | InnerNode a (Bracket a) (Bracket a)

-- Map a function over all values in the bracket
mapBracket : (a -> a) -> Bracket a -> Bracket a
mapBracket fun b = case b of
  Leaf a -> Leaf (fun a)
  InnerNode a b1 b2 -> InnerNode (fun a) (mapBracket fun b1) (mapBracket fun b2)

anyBracket : (a -> Bool) -> Bracket a -> Bool
anyBracket fun b = case b of
                     Leaf a -> (fun a)
                     InnerNode a b1 b2 -> (fun a) || anyBracket fun b1 || anyBracket fun b2

renderBracket : Bracket a -> (a -> (Int, Int, Form)) -> (Int, Int, [Form])
renderBracket b draw =
  case b of
    Leaf match ->
      let (w,h,drawn) = draw match in
      (w,h,[drawn])
    InnerNode match bl br ->
      let arrstyle      = {defaultLine | width <- 5, join <- Smooth}
          (w1,h1,left)  = renderBracket bl draw
          (w2,h2,right) = renderBracket br draw
          (w,h, drawn)  = draw match
          left'         = map (move (toFloat (-w - 50), toFloat -h1)) left
          right'        = map (move (toFloat (-w - 50), toFloat h2)) right
          tarrw         = traced arrstyle
                           (path [(toFloat -w/2 - 50, toFloat -h1),
                                 (toFloat -w/2 - 25, toFloat -h1),
                                 (toFloat -w/2 - 25,0)])
          barrw         = traced arrstyle
                           (path [(toFloat -w/2 - 50, toFloat h2),
                                 (toFloat -w/2 - 25, toFloat h2),
                                 (toFloat -w/2 - 25,0),
                                 (toFloat -w/2,0)])
      in
      (w+w1,
       h1+h2,
       drawn :: tarrw :: barrw :: (left' ++ right'))
