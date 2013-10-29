import Color
import Bracket (InnerNode,Leaf,renderBracket)
import Either (Either,Left,Right)

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
renderMatch m = group [renderBrack m.top |> moveY 10,
                       renderBrack m.bottom |> moveY -10]

alwaysRect : Either Match Brack -> (Int, Int, Form)
alwaysRect m = case m of
                Left  m -> (200, 40,
                            group [rect 200 40 |> outlined (solid black),
                                   renderMatch m,
                                   traced (solid black) [(-100,0),(100,0)]])
                Right b -> (200, 20,
                            renderBrack b)



(bwidth, bheight, bracket) = renderBracket b alwaysRect
main = [rect 800 800 |> filled blue,
        bracket
        |> moveX (toFloat bwidth/2 + 50)]
       |> collage 800 800
