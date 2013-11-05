module Render where
import open Bracket
import Maybe

txt : (Text -> Text) -> String -> Element
txt f = text . f . toText

type Selection = { brack: Maybe Brack }

renderBrack : Selection -> Brack -> Bool -> Element
renderBrack sel b winner =
  let contain = container 180 20
      text = if winner then txt bold else txt id
  in  layers
            (Maybe.cons (maybe Nothing (\b' -> if brackEq b' b then Just (spacer 200 20 |> color red) else Nothing) sel.brack)
            [ text (.name b)
               |> contain midLeft,
               plainText (show (.score b))
               |> contain midRight])

renderMatch' : Selection -> Match -> Brack -> Brack -> (Int,Int,Form)
renderMatch' sel m p1 p2 = let cont pos elm = container 200 80 pos elm
                               img  s       = image 200 20 s
                               winner       = maxScore m
                               p1won        = maybe False (\b -> brackEq b p1) winner
                               p2won        = maybe False (\b -> brackEq b p2) winner
                               remaining    = not p1won && not p2won
                                              && not (.name p1 /= "Empty!")
                                              && not (.name p2 /= "Empty!")
                               finished     = not remaining
                               inprocCol    = rgba 0 255 0 0.5
                               finishedCol  = rgba 79 54 153 0.5
                               remainingCol = rgba 79 54 153 0.2
                               backColor    = if not remaining then inprocCol
                                                               else remainingCol
                       in
                       (200,50, layers
                                  [ cont middle (spacer 200 65
                                                 |> color backColor),
                                    cont midTop (img "top.png"),
                                    cont midBottom (img "bottom.png"),
                                    cont middle (flow down [(renderBrack sel p1 p1won),
                                                            (renderBrack sel p2 p2won)])]
                                |> toForm)


renderMatch : Selection -> Match -> (Int, Int, Form)
renderMatch sel m =
  let fake = player "Empty!"
  in case m of
    Empty ->     renderMatch' sel m fake fake
    One p ->     renderMatch' sel m p fake
    Two p1 p2 -> renderMatch' sel m p1 p2

renderBracket : Selection -> Bracket -> (Int, Int, [Form])
renderBracket sel b =
  case b of
    Leaf match ->
      let (w,h,drawn) = renderMatch sel match in
      (w,h,[drawn])
    InnerNode match bl br ->
      let (w1,h1,left)  = renderBracket sel bl
          (w2,h2,right) = renderBracket sel br
          (w,h, drawn)  = renderMatch sel match
          left'         = map (move (toFloat (-w - 50), toFloat -h1)) left
          right'        = map (move (toFloat (-w - 50), toFloat h2))  right
          tarrw         = traced lineStyle
                           (path [(toFloat -w/2 - 50, toFloat -h1),
                                  (toFloat -w/2 - 25, toFloat -h1),
                                  (toFloat -w/2 - 25,0)])
          barrw         = traced lineStyle
                           (path [(toFloat -w/2 - 50, toFloat h2),
                                  (toFloat -w/2  - 25, toFloat h2),
                                  (toFloat -w/2  - 25,0),
                                  (toFloat -w/2, 0)])
      in
      (w+w1,
       h1+h2,
       drawn :: tarrw :: barrw :: (left' ++ right'))
