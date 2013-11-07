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
import Marshal

style = BracketStyle Single (BestOf 3)

players = map (\i -> "Player " ++ (show i)) [1..16]
initialBracket = fromList style players

stepBracket : Input -> Bracket -> Bracket
stepBracket inp b = b

foreign import jsevent "action" (fromString "nop")
  action : Signal JSString

type Input = { dir:{x:Int,y:Int},
               space: Bool,
               action: JSString }
input = Input <~ Keyboard.arrows
                 ~ Keyboard.space
                 ~ action

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
