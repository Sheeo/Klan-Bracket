import Color
import open Bracket
import Either (Either,Left,Right)
import Window
import Maybe
import Keyboard
import Graphics.Input (hoverable)


renderBrack : Brack -> Element
renderBrack b = let contain = container 200 20
                in
                    layers
                          (Maybe.cons (if .selected b
                                       then Just (spacer 200 20 |> color red)
                                       else Nothing)
                          [ plainText (.name b)
                             |> contain midLeft,
                             plainText (show (.score b))
                             |> contain midRight])

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
                  (Just w1, Just w2) -> Two {w2 | score <- 0} {w1 | score <- 0}
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

-- Selection updates
selectFirst : Bracket Match -> Bracket Match
selectFirst b = case b of
                  Leaf m -> case m of
                              Empty -> Leaf Empty
                              One b -> Leaf (One {b | selected <- True})
                              Two b1 b2 -> Leaf (Two {b1 | selected <- True} b2)
                  InnerNode m b1 b2 -> InnerNode m b1 (selectFirst b2)


moveSelected : Direction -> Bracket Match -> Bracket Match
moveSelected d b = b

players = map (\i -> "Player " ++ (show i)) [1..16]
initialBracket = fromList players

type Input = { dir:{x:Int,y:Int}, space:Bool }
input = Input <~ Keyboard.arrows ~ Keyboard.space

stepBracket : Input -> Bracket Match -> Bracket Match
stepBracket inp b = let anySelected =
                          anyBracket (\m -> anyMatch .selected m) b
                    in
                    if | anySelected == False -> selectFirst b
                       | inp.dir.x == 1 -> moveSelected right b
                       | inp.dir.x == -1 -> moveSelected left b
                       | inp.dir.y == 1 -> moveSelected up b
                       | inp.dir.y == -1 -> moveSelected down b

bracketState = foldp stepBracket initialBracket input

render : (Int,Int) -> Bracket Match -> Element
render input bracket =
      let (bw,bh,brkt) = renderBracket bracket renderMatch
          (w,h) = (fst input, snd input)
          br = map (moveX (toFloat bw/2)) brkt
      in
      ([rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png") |> toForm]
      ++ [group br])
       |> collage w h

main = lift2 render Window.dimensions bracketState
