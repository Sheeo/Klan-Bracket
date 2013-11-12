module Marshal where
import open Bracket

import open JavaScript
import open Json
import JavaScript.Experimental as JS

-- Very tedious marshalling code

bracketToJS : Bracket -> JSObject
bracketToJS bracket = JS.fromRecord {}

matchFromJS m =
  case .title m of
    "" -> emptyMatch ""
    _ -> emptyMatch ""

bracketFromJS o =
  case .t o of
    "Leaf" -> Just (Leaf (emptyMatch "1"))
    _ -> Nothing

data Action = ResetBracket String
            | AddBracket String
            | RemoveBracket String
            | AddPlayer String String
            | Nop

actionFromJSObject jso =
  case jso.actiontype of
    _ -> Nop

type JSInput = { action: Action }

defaultJSObject = JS.fromRecord { actiontype = "nop" }

fromJS : JSObject -> JSInput
fromJS obj =
  let inp = JS.toRecord obj
  in { action = actionFromJSObject inp.actiontype }
