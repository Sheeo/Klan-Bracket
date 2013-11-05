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
            [ text (show (.name b))
               |> contain midLeft,
               plainText (show (.score b))
               |> contain midRight])

renderMatch : Selection -> Match -> (Int,Int,Form)
renderMatch sel m = let cont pos elm = container 200 80 pos elm
                        img  s       = image 200 20 s
                        winner       = Nothing --maxScore m
                        p1won        = maybe False (\b -> brackEq b (.top m)) winner
                        p2won        = maybe False (\b -> brackEq b (.bottom m)) winner
                        remaining    = not p1won && not p2won
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
                                    cont middle (flow down [(renderBrack sel (.top m) p1won),
                                                            (renderBrack sel (.bottom m) p2won)])]
                                |> toForm)

renderBracket : Selection -> Bracket -> (Int, Int, [Form])
renderBracket sel b =
  let gayPurple = solid (rgb 79 54 153)
      lineStyle = { gayPurple | width <- 3,
                                cap <- Round,
                                join <- Sharp 0.1}
  in case b of
    Leaf match ->
      let (w,h,drawn) = renderMatch sel match in
      (w,h,[drawn])
    InnerNode match top bottom ->
      let (w1,h1,right)  = renderBracket sel top
          (w2,h2,left) = renderBracket sel bottom
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
