module Edge exposing (..)

import Model exposing (Edge, Triangle)


getUnique : List Edge -> List Edge
getUnique edges =
    let
        duplicates =
            getDuplicates edges
    in
    List.filter (\x -> Basics.not (contains duplicates x)) edges


getDuplicates : List Edge -> List Edge
getDuplicates edges =
    case List.tail edges of
        Nothing ->
            []

        Just tail ->
            case List.head edges of
                Nothing ->
                    []

                Just head ->
                    List.append
                        (getDuplicate tail head)
                        (getDuplicates (List.drop 1 edges))


getDuplicate : List Edge -> Edge -> List Edge
getDuplicate edges edge =
    if contains edges edge then
        [ edge ]
    else
        []


contains : List Edge -> Edge -> Bool
contains edges edge =
    List.any (\x -> isEqual edge x) edges


isEqual : Edge -> Edge -> Bool
isEqual a b =
    if (a.a == b.a) && (a.b == b.b) then
        True
    else if (a.a == b.b) && (a.b == b.a) then
        True
    else
        False
