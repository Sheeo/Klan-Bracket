import Color
import Bracket (Brack,Match,Player,InnerNode,Leaf,Brack_desc,brackName,brackScore,renderBracket)
import Either (Either,Left,Right)

player string = Player (Brack_desc 0 string)

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
renderBrack b = group [rect 200 20 |> filled white,
                       toForm (plainText (brackName b)) |> moveX -65,
                       toForm (plainText (show (brackScore b))) |> moveX 80]

renderMatch : Match -> Form
renderMatch m = group [renderBrack m.top |> moveY 10,
                       renderBrack m.bottom |> moveY -10]

alwaysRect : Either Match Brack -> (Int, Int, Form)
alwaysRect m = case m of
                Left  m -> (200, 40,
                            group [rect 200 40 |> outlined (solid black),
                                   renderMatch m,
                                   traced (solid black) [(-100,0),(100,0)]])
                Right b -> (200, 40,
                            renderBrack b)



main = [rect 800 800 |> filled blue,
        renderBracket b alwaysRect
        |> moveX 200
        |> scale 0.7]
       |> collage 800 800
