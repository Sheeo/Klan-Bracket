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
data Bracket a = Empty
               | MatchNode a (Bracket a) (Bracket a)

renderBracket : Bracket (Either Match Brack) -> ((Either Match Brack) -> Form) -> Form
-- Above type is intentionally specific to avoid unification bug
renderBracket b draw =
        let render : Bracket (Either Match Brack) -> Form -> (Int, Form)
            render b f =
              case b of
                Empty -> (0,f)
                MatchNode match bl br ->
                  let (h1,left)  = render bl f
                      (h2,right) = render br f
                  in
                  (h1 + h2 + 30,
                  group
                     [draw match,
                      left  |> move (toFloat -240, toFloat -h1),
                      right |> move (toFloat -240, toFloat  h2),
                      (traced defaultLine (path [(-140, toFloat -h1),
                                                 (-90,0),
                                                 (0,0)])),
                      (traced defaultLine (path [(-140, toFloat h2),
                                                 (-90,0),
                                                 (0,0)]))
                      ])
            (_,r) = render b (spacer 0 0 |> toForm)
        in r

mat : Bracket (Either Match Brack)
mat = (MatchNode
          (Left (Match
            (player "Player 1")
            (player "Player 2")))
         (MatchNode
            (Left (Match
              (player "Player 1")
              (player "Player 2")))
            Empty Empty)
         (MatchNode
            (Left (Match
              (player "Player 1")
              (player "Player 2")))
            Empty Empty))

b : Bracket (Either Match Brack)
b = MatchNode (Right (player "Unknown"))
         (MatchNode
            (Left (Match
              (player "Player 1")
              (player "Player 2")))
            mat mat)
         (MatchNode
            (Left (Match
              (player "Player 1")
              (player "Player 2")))
            mat mat)

alwaysRect : Either a b -> Form
alwaysRect m = case m of
                Left m -> rect 200 40 |> filled Color.black
                Right b -> rect 200 20 |> filled Color.black

main = [move (200, 0) (renderBracket b alwaysRect) |> scale 0.5] |>
       collage 600 600
