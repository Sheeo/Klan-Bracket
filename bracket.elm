module Bracket where

-- Tree bracket structure
data Bracket a = Leaf a
               | InnerNode a (Bracket a) (Bracket a)

renderBracket : Bracket a -> (a -> (Int, Int, Form)) -> (Int, Int, Form)
renderBracket b draw =
        let render b =
              case b of
                Leaf match ->
                  let (w,h,drawn) = draw match in
                  (w,h,drawn)
                InnerNode match bl br ->
                  let (w1,h1,left)  = render bl
                      (w2,h2,right) = render br
                      (w,h, drawn) = draw match
                  in
                  (w+w1,
                  h1+h2,
                  group
                     [drawn,
                      left  |> move (toFloat (-w - 50), toFloat -h1),
                      right |> move (toFloat (-w - 50), toFloat  h2),
                      (traced (dashed black)
                              (path [(toFloat -w/2 - 50, toFloat -h1),
                                     (toFloat -w/2 - 25, toFloat -h1),
                                     (toFloat -w/2 - 25,0)])),
                      (traced (dashed black)
                              (path [(toFloat -w/2 - 50, toFloat h2),
                                     (toFloat -w/2 - 25, toFloat h2),
                                     (toFloat -w/2 - 25,0),
                                     (toFloat -w/2,0)]))
                      ])
        in render b
