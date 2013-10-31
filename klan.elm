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
            (Left (Match
              (player "Test")
              (player "P2")))
            mat mat)

renderBrack : Brack -> Element
renderBrack b = plainText (brackName b)

renderMatch : Match -> Form
renderMatch m = container 200 20 midLeft (renderBrack m.top)
                ::
                [container 200 20 midLeft (renderBrack m.bottom)]
                |> flow down
                |> toForm

renderMatchBrack : Either Match Brack -> (Int, Int, Form)
renderMatchBrack m = case m of
                Left  m -> (200, 40,
                            group [rect 200 40 |> outlined (solid black),
                                   renderMatch m,
                                   traced (solid black) [(-100,0),(100,0)]])
                Right b -> (200, 20,
                            toForm <| renderBrack b)

render : (Int,Int) -> Element
render input =
      let (bw,bh,brkt) = renderBracket b renderMatchBrack
          (w,h) = (fst input, snd input)
          br = map (moveX (toFloat bw/2 + 50)) brkt
      in
      ([rect (toFloat w) (toFloat h) |> filled white,
        container w h midTop (image 400 100 "Banner4.png") |> toForm]
      ++ [group br])
       |> collage w h

main = lift render Window.dimensions
