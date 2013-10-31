import Color
import Bracket (Bracket,InnerNode,Leaf,renderBracket,mapBracket)
import Either (Either,Left,Right)
import Window
import Graphics.Input (hoverable)

player string = Player (Brack_desc 0 string)
playerWithScore name score = Player (Brack_desc score name)

-- A 'brack' is a name of either a player or a team,
-- associated with a score
type Brack_desc = { score:Int,
                    name:String }

brackName : Brack -> String
brackName b = case b of
                  Player d -> .name d
                  Team d -> .name d

brackScore : Brack -> Int
brackScore b = case b of
                  Player p -> .score p
                  Team t -> .score t

resetBrackScore : Brack -> Brack
resetBrackScore b = case b of
                  Player p -> Player { p | score <- 0}
                  Team   t -> Team { t | score <- 0}

data Brack = Player Brack_desc
           | Team Brack_desc

data Match = Empty
           | One Brack
           | Two Brack Brack

brackEq b1 b2 = brackName b1 == brackName b2

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
              Two b1 b2 -> if | brackScore b1 > brackScore b2 -> Just b1
                              | brackScore b2 > brackScore b1 -> Just b2
                              | otherwise -> Nothing

renderBrack : Brack -> Element
renderBrack b = let contain = container 200 20
                    (elm,signal) = hoverable (layers [ plainText (brackName b)
                                   |> contain midLeft,
                                   plainText (show (brackScore b))
                                   |> contain midRight])
                in elm

renderMatch : Match -> (Int, Int, Form)
renderMatch m =
  case m of
    Empty ->     (200,0,  spacer 0 0 |> toForm)
    One p ->     (200,20, renderBrack p |> toForm)
    Two p1 p2 -> let cont = container 200 20 midLeft
                 in
                 (200,40, cont (renderBrack p1)
                          ::
                          [cont (renderBrack p2)]
                          |> flow down
                          |> toForm)

-- Build an initial bracket from a list of names
fromList : [String] -> Bracket Match
fromList players =
  let minDepth : Bracket Match -> Int
      minDepth b = case b of
          Leaf _ -> 0
          InnerNode _ b1 b2 -> 1 + min (minDepth b1) (minDepth b2)
      hasFreeSpot : Bracket Match -> Bool
      hasFreeSpot b = case b of
          Leaf m -> case m of
                      Empty -> True
                      One _ -> True
                      _ -> False
          InnerNode m b1 b2 -> hasFreeSpot b1 || hasFreeSpot b2
      chooseSide : String -> Bracket Match -> Bracket Match
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
      consBracket : String -> Bracket Match -> Bracket Match
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
      build : [String] -> Bracket Match -> Bracket Match
      build ps b =
        case ps of
          p1 :: [] -> consBracket p1 b
          p1 :: ps' -> build ps' (consBracket p1 b)
  in build players (Leaf Empty)

winners : Bracket Match -> Bracket Match -> Match
winners b1 b2 = let winner : Bracket Match -> Maybe Brack
                    winner b =
                      case b of
                        Leaf m -> maxScore m
                        InnerNode m b1 b2 -> maxScore m
                in
                case ((winner b1), (winner b2)) of
                  (Just w1, Just w2) -> Two (resetBrackScore w2) (resetBrackScore w1)
                  _ -> Empty


-- Update a bracket with matches determined from nonempty nodes
updateBracket : Bracket Match -> Bracket Match
updateBracket b = case b of
                    Leaf m -> b
                    InnerNode m b1 b2 ->
                      let b1' = updateBracket b1
                          b2' = updateBracket b2
                      in
                      case m of
                        Empty -> InnerNode (winners b1' b2') b1' b2'
                        _ -> b

-- Given a match, update it in the bracket
updateScore : Match -> Bracket Match -> Bracket Match
updateScore m b = mapBracket (\m' -> if matchEq m m' then m
                                     else m')
                              b


players = map (\i -> "Player " ++ (show i)) [1..12]
bracket = fromList players
          |> updateScore (Two (player "Player 7") (playerWithScore "Player 8" 2))
          |> updateScore (Two (playerWithScore "Player 5" 2) (player "Player 6"))
          |> updateScore (Two (playerWithScore "Player 1" 2) (player "Player 2"))
          |> updateScore (Two (playerWithScore "Player 3" 2) (player "Player 4"))
          |> updateBracket
          |> updateScore (Two (playerWithScore "Player 1" 2) (player "Player 3"))
          |> updateScore (Two (playerWithScore "Player 5" 2) (player "Player 8"))
          |> updateBracket

render : (Int,Int) -> Element
render input =
      let (bw,bh,brkt) = renderBracket bracket renderMatch
          (w,h) = (fst input, snd input)
          br = map (moveX (toFloat bw/2)) brkt
      in
      ([rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png") |> toForm]
      ++ [group br])
       |> collage w h

main = lift render Window.dimensions
