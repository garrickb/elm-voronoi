module Edge exposing (..)

import Model exposing (Edge)


isEqual : Edge -> Edge -> Bool
isEqual a b =
    if (a.a == b.a) && (a.b == b.b) then
        True
    else if (a.a == b.b) && (a.b == b.a) then
        True
    else
        False
