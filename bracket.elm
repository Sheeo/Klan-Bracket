module Bracket where

-- Tree bracket structure
data Bracket a = Leaf a
               | InnerNode a (Bracket a) (Bracket a)

renderBracket : Bracket a -> (a -> (Int, Int, Form)) -> (Int, Int, [Form])
renderBracket b draw =
  case b of
    Leaf match ->
      let (w,h,drawn) = draw match in
      (w,h,[drawn])
    InnerNode match bl br ->
      let (w1,h1,left)  = renderBracket bl draw
          (w2,h2,right) = renderBracket br draw
          (w,h, drawn)  = draw match
          left'         = map (move (toFloat (-w - 50), toFloat -h1)) left
          right'        = map (move (toFloat (-w - 50), toFloat h2)) right
          tarrw         = traced (dashed black)
                           (path [(toFloat -w/2 - 50, toFloat -h1),
                                 (toFloat -w/2 - 25, toFloat -h1),
                                 (toFloat -w/2 - 25,0)])
          barrw         = traced (dashed black)
                           (path [(toFloat -w/2 - 50, toFloat h2),
                                 (toFloat -w/2 - 25, toFloat h2),
                                 (toFloat -w/2 - 25,0),
                                 (toFloat -w/2,0)])
      in
      (w+w1,
       h1+h2,
       drawn :: tarrw :: barrw :: (left' ++ right'))
