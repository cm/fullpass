module UIKit exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (..)
import Json.Decode as Json
import Messages exposing (..)
import Models exposing (..)
import String.Extra exposing (..)


adminLayoutView : List Link -> List (Html Msg) -> List (Html Msg) -> Html Msg
adminLayoutView global local main =
    div [ class "wrapper layout-admin" ]
        [ div [ class "row" ]
            [ div [ class "col-md-1 layout-global" ]
                (List.map globalIconView global)
            , div [ class "col-md-5 layout-local", attribute "data-background-color" "orange" ]
                local
            , div [ class "col-md-6 layout-main" ]
                main
            ]
        ]


type alias Link =
    { title : String
    , icon : Maybe String
    , action : Msg
    , active : Bool
    , counter : Int
    }


nucleoIcon : String -> Html Msg
nucleoIcon style =
    "now-ui-icons " ++ style |> icon


icon : String -> Html Msg
icon style =
    i [ class style ] []


iconOrDefault : Maybe String -> String
iconOrDefault icon =
    case icon of
        Nothing ->
            ""

        Just i ->
            i


activeClass : Bool -> String
activeClass active =
    case active of
        True ->
            "active"

        False ->
            ""


globalIconView : Link -> Html Msg
globalIconView link =
    a [ href "#", onClick link.action, class (activeClass link.active) ]
        [ link.icon |> iconOrDefault |> nucleoIcon
        , p [] [ text link.title ]
        ]


onClick : msg -> Attribute msg
onClick msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed msg)
