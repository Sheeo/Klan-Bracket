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

type JSInput = {action: String, bracket: Maybe Bracket}

defaultJSObject = JS.fromRecord {action="nop", bracket = {}}

fromJS : JSObject -> JSInput
fromJS obj =
  let inp = JS.toRecord obj
  in { action = inp.action,
       bracket = bracketFromJS inp.bracket }
