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
data Bracket = Empty
             | MatchNode (Either Match Brack) Bracket Bracket

brackText : String -> Element
brackText name = text (Text.toText name)

renderBrack : Int -> Int -> Brack -> Form
renderBrack w h name = container w (h `div` 2)
                                  midLeft
                                  (brackText (brackName name))
                       |> toForm

renderBracket : Bracket -> Form
renderBracket b =
        let render : Bracket -> Form -> (Int,Int,Form)
            render b f =
              case b of
                Empty -> (0,0,f)
                MatchNode match bl br ->
                  let (w1,h1,left) = render bl f
                      (w2,h2,right) = render br f
                  in
                  (240,
                   h1 + h2 + 30,
                  group
                     [rect 200 50 |> filled Color.black,
                      left  |> move (toFloat -w1,toFloat -h1),
                      right |> move (toFloat -w1, toFloat h2)])
            (_,_,r) = render b (rect 0 0 |> filled Color.white)
        in r

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

main = [move (200, 0) (renderBracket b) |> scale 0.5] |>
       collage 600 600
