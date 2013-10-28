import Color
import Either (Either,Left,Right)

-- A 'brack' is a name of either a player or a team,
-- associated with a score
type Brack_desc = {score:Int, name:String}

brackName : Brack -> String
brackName b = case b of
                  Player d -> .name d
                  Team d -> .name d

data Brack = Player Brack_desc
           | Team Brack_desc

player string = Player (Brack_desc 0 string)

-- A match is a matchup between two bracks
type Match = { top:Brack, bottom:Brack }

-- The full bracket is a tree
data Bracket a = Leaf a
               | InnerNode a (Bracket a) (Bracket a)

renderBracket : Bracket (Either Match Brack)   ->
                ((Either Match Brack) -> (Int, Int, Form)) ->
                Form
-- Above type is intentionally specific to avoid unification bug
renderBracket b draw =
        let render : Bracket (Either Match Brack) -> (Int, Int, Form)
            render b =
              case b of
                Leaf match ->
                  let (w,h,drawn) = draw match in
                  (w,h,drawn)
                InnerNode match bl br ->
                  let (_,h1,left)  = render bl
                      (_,h2,right) = render br
                      (w,_, drawn) = draw match
                  in
                  (w,
                  h1 + h2 + 30,
                  group
                     [drawn,
                      left  |> move (toFloat (-w - 50), toFloat -h1),
                      right |> move (toFloat (-w - 50), toFloat  h2),
                      (traced (dashed Color.black)
                              (path [(toFloat -w/2 - 50, toFloat -h1),
                                     (toFloat -w/2 - 25, toFloat -h1),
                                     (toFloat -w/2 - 25,0)])),
                      (traced (dashed Color.black)
                              (path [(toFloat -w/2 - 50, toFloat h2),
                                     (toFloat -w/2 - 25, toFloat h2),
                                     (toFloat -w/2 - 25,0),
                                     (toFloat -w/2,0)]))
                      ])
            (_,_,r) = render b
        in r

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
                       toForm (plainText (brackName b)) ]

renderMatch : Match -> Form
renderMatch m = group [renderBrack m.top |> moveY 10,
                       renderBrack m.bottom |> moveY -10]

alwaysRect : Either Match Brack -> (Int, Int, Form)
alwaysRect m = case m of
                Left  m -> (200, 40,
                            renderMatch m)
                Right b -> (200, 40,
                            renderBrack b)

main = [rect 800 800 |> filled blue,
        renderBracket b alwaysRect |> moveX 200 |> scale 0.7]
       |> collage 800 800
