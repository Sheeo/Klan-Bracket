import Color
import Bracket (Bracket,InnerNode,Leaf,renderBracket)
import Either (Either,Left,Right)
import Window

player string = Player (Brack_desc 0 string)

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

data Brack = Player Brack_desc
           | Team Brack_desc

data Match = Empty
           | One Brack
           | Two Brack Brack

renderBrack : Brack -> Element
renderBrack b = let contain = container 200 20
                in
                layers [ plainText (brackName b)
                         |> contain midLeft,
                         plainText (show (brackScore b))
                         |> contain midRight]

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

players = map (\i -> "Player " ++ (show i)) [1..16]

b = fromList players

render : (Int,Int) -> Element
render input =
      let (bw,bh,brkt) = renderBracket b renderMatch
          (w,h) = (fst input, snd input)
          br = map (moveX (toFloat bw/2)) brkt
      in
      ([rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png") |> toForm]
      ++ [group br])
       |> collage w h

main = lift render Window.dimensions
