import Color
import open Bracket
import open Render
import Either (Either,Left,Right)
import Window
import Maybe
import Keyboard
import Graphics.Input (hoverable)
import JavaScript (JSString,fromString)
import Text


winners : Bracket -> Bracket -> Match
winners b1 b2 = let winner : Bracket -> Maybe Brack
                    winner b =
                      case b of
                        Leaf m -> maxScore m
                        InnerNode m b1 b2 ->
                          let ownmax = maxScore m
                              b1max  = maxScore (match b1)
                              b2max  = maxScore (match b2)
                          in if | isJust ownmax -> ownmax
                                | isJust b1max -> b1max
                                | isJust b2max -> b2max
                                | otherwise -> Nothing
                in
                case ((winner b1), (winner b2)) of
                  (Just w1, Just w2) -> Two {w2 | score <- 0}
                                            {w1 | score <- 0}
                  (Just w1,Nothing) ->  One {w1 | score <- 0}
                  (Nothing,Just w2) ->  One {w2 | score <- 0}
                  _ -> Empty


-- Update a bracket with matches determined from nonempty nodes
updateBracket : Bracket -> Bracket
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
updateScore : Match -> Bracket -> Bracket
updateScore m b = mapBracket (\m' -> if matchEq m m' then m
                                     else m')
                              b
                 |> updateBracket

players = map (\i -> "Player " ++ (show i)) [1..16]
initialBracket = fromList players
                 |> updateScore (Two (playerWithScore "Player 1" 1)
                                     (playerWithScore "Player 2" 3))
                 |> updateScore (Two (player "Player 3")
                                     (playerWithScore "Player 4" 3))
                 |> updateScore (Two (player "Player 2")
                                     (playerWithScore "Player 4" 3))
                 |> updateScore (Two (player "Player 5")
                                     (playerWithScore "Player 6" 3))
                 |> updateScore (Two (player "Player 7")
                                     (playerWithScore "Player 8" 3))
                 |> updateScore (Two (player "Player 6")
                                     (playerWithScore "Player 8" 3))
                 |> updateScore (Two (player "Player 4")
                                     (playerWithScore "Player 8" 3))

stepBracket : Input -> Bracket -> Bracket
stepBracket inp b = b

type Input = { dir:{x:Int,y:Int}, space:Bool }
input = Input <~ Keyboard.arrows ~ Keyboard.space

bracketState : Signal (Bracket)
bracketState = foldp stepBracket initialBracket input

render : Selection -> (Int,Int) -> Bracket -> Element
render sel input bracket =
      let (bw,bh,brkt) = renderBracket sel bracket
          (w,h) = (fst input, snd input)
          br = map (moveX (toFloat bw/2)) brkt
      in
      ([rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png") |> toForm]
      ++ [group br])
       |> collage w h

printState : (Int,Int) -> Bracket -> JSString
printState d b = fromString <| "Rendered at " ++ show d ++ " " ++ show b

log = lift2 printState Window.dimensions (dropRepeats bracketState)

foreign export jsevent "log"
  log : Signal JSString

main = lift3 render (constant {brack=Nothing}) Window.dimensions (dropRepeats bracketState)
