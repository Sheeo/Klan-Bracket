import Color

-- A 'brack' is a name of either a player or a team,
-- associated with a score
type Brack_desc = {score:Int, name:String}

brackName : Brack -> String
brackName b = case b of
                  Player d -> .name d
                  Team d -> .name d

data Brack = Player Brack_desc
           | Team Brack_desc

-- A match is a matchup between two bracks
type Match = { top:Brack, bottom:Brack }

-- Rendering
txt = text . Text.toText

box : (Int,Int) -> Form
box s = rect (toFloat (fst s)) (toFloat (snd s))
        |> outlined (solid Color.black)

centerline w = traced (dashed Color.black)
  <| path [(-(w/2),0),(w/2,0)]

toFloatBoth (a,b) = (toFloat a, toFloat b)

playerText : String -> Element
playerText name = txt name

bracketTextBox : Int -> Int -> Brack -> Element
bracketTextBox w h name = container w (h `div` 2)
                                  midLeft
                                  (playerText (brackName name))

matchBox : Int -> Element -> Form
matchBox s e = group ((rect 200 50 |> filled Color.green)
                      :: (toForm e :: [box (200,50), centerline 200]))
               |> move(-170,toFloat (-60*s))

brackColumn : [Match] -> [Form]
brackColumn matches =
  let brack = (bracketTextBox 200 50)
  in
    map (\({top,bottom},i) ->
          matchBox i (flow down [brack top,
                                 brack bottom]))
        (zip matches [0..(length matches)])

-- Temporary construction of matches

toMatch : String -> String -> Match
toMatch t b = Match ((Player . Brack_desc 0) t)
                    ((Player . Brack_desc 0) b)

brackets : [Form]
brackets = brackColumn <| [toMatch "Player 1" "Player 2",
                           toMatch "Player 3" "Player 4",
                           toMatch "Player 5" "Player 6",
                           toMatch "Player 7" "Player 8"]
          

main = (rect 600 600 |> filled Color.lightCharcoal)
       :: brackets
     |> collage 600 600
