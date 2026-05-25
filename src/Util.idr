module Util

import Data.Vect

public export
joinBy : Vect n String -> String -> String
joinBy Nil _ = ""
joinBy [x] by = x
joinBy (x::xs) by = x ++ by ++ joinBy xs by
