import Color
import Bracket (InnerNode,Leaf,renderBracket)
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

type Match = { top:Brack, bottom:Brack }

mat = (InnerNode
          (Left (Match
            (player "Player 1")
            (player "Player 2")))
         (Leaf
            (Left (Match
              (player "Player 1")
              (player "Player 2"))))
         (Leaf
            (Left (Match
              (player "Player 1")
              (player "Player 2")))))

b = InnerNode (Right (player "Unknown"))
         (InnerNode
            (Left (Match
              (player "Player 1")
              (player "Player 2")))
            mat mat)
         (InnerNode
            (Right (player "Test"))
            mat mat)

renderBrack : Brack -> Form
renderBrack b = let color = case (brackScore b) of
                            0 -> white
                            _ -> green
                in
                group [rect 200 20 |> filled white,
                       toForm (plainText (brackName b)) |> moveX -65,
                       toForm (plainText (show (brackScore b))) |> moveX 80]

renderMatch : Match -> Form
renderMatch m = group [renderBrack m.top
                        |> moveY 10,
                       renderBrack m.bottom
                        |> moveY -10]

renderMatchBrack : Either Match Brack -> (Int, Int, Form)
renderMatchBrack m = case m of
                Left  m -> (200, 40,
                            group [rect 200 40 |> outlined (solid black),
                                   renderMatch m,
                                   traced (solid black) [(-100,0),(100,0)]])
                Right b -> (200, 20,
                            renderBrack b)

render : (Int,Int) -> Element
render input =
      let (bwidth,bheight,bracket) = renderBracket b renderMatchBrack
          (w,h) = (fst input, snd input)
      in
      [rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png")
        |> toForm,
        bracket
        |> moveX (toFloat bwidth/2 + 50)]
       |> collage w h

main = lift render Window.dimensions
