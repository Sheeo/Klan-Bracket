import Color
import open Bracket
import open Render
import Either (Either,Left,Right)
import Window
import Maybe
import Keyboard
import Graphics.Input (hoverable)
import Text
import JavaScript (JSString,JSObject,fromString)
import JavaScript.Experimental as JS
import open Marshal

style = BracketStyle Single (BestOf 3)

players = map (\i -> "Player " ++ (show i)) [1..8]
initialBracket = fromList style []

foreign import jsevent "action" (JS.fromRecord {actiontype="nop"})
  action : Signal JSObject

type Input = { dir:{x:Int,y:Int},
               space: Bool,
               jsinput: JSInput }
input = Input <~ Keyboard.arrows
                 ~ Keyboard.space
                 ~ (fromJS <~ action)

stepBracket : Input -> Bracket -> Bracket
stepBracket ({dir,space,jsinput} as inp) b =
  case .action jsinput of
    ResetBracket title -> b
    AddPlayer bracketTitle playerName -> b
    _ -> b

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
printState d b = fromString <| "Rendered at " ++ show d

log = lift2 printState Window.dimensions bracketState

foreign export jsevent "bracketLog"
  log : Signal JSString

main : Signal Element
main = lift3 render (constant {brack=Nothing}) Window.dimensions (dropRepeats bracketState)
