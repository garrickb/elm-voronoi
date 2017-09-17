module View exposing (..)

import Html
import Model
import Update


view : Model.Model -> Html.Html Update.Msg
view model =
    Html.div
        []
        [ Html.text "Hello World" ]
