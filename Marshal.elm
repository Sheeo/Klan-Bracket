module Marshal where
import open Bracket

import open JavaScript
import open JavaScript.Experimental

toJS : Bracket -> JSObject
toJS bracket = fromRecord {}
