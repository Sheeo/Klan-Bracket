module Bracket where
import Either (Either,Left,Right)
import Color

-- A 'brack' is a name of either a player or a team,
-- associated with a score
type Brack_desc = {score:Int, name:String}

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
