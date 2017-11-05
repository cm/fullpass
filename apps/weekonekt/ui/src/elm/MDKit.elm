module MDKit exposing (..)

import FileReader exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (..)
import Json.Decode as Json
import Messages exposing (..)
import Models exposing (..)
import String.Extra exposing (..)


mdCardView : MDCard -> Html Msg
mdCardView card =
    let
        contents =
            [ div [ class "card-content" ]
                [ card.contents ]
            ]

        tabs =
            []

        title =
            []

        actions =
            []
    in
    div [ class "card" ]
        (title ++ tabs ++ contents ++ actions)


mdActiveClass : Bool -> String
mdActiveClass v =
    case v of
        True ->
            "active"

        False ->
            ""


mdRowView : String -> Html Msg -> Html Msg
mdRowView placement contents =
    div [ class "row" ]
        [ div [ class ("col s12 " ++ placement ++ "-align") ]
            [ contents ]
        ]


mdLinkView : MDLink -> Html Msg
mdLinkView link =
    p []
        [ a [ href "#", onClick link.action ] [ text link.title ]
        ]


mdFloatingButtonView : MDLink -> Html Msg
mdFloatingButtonView link =
    let
        icon =
            case link.icon of
                Just i ->
                    i

                Nothing ->
                    { name = "more_vert", position = "center" }
    in
    a [ class "btn-floating btn-large waves-effect waves-light red", onClick link.action ]
        [ icon |> mdIconView ]


mdFormSectionView : MDFormSection -> Html Msg
mdFormSectionView s =
    div [ class "container center-align" ]
        [ h4 [ class "center-align grey-text" ] [ text s.title ]
        , { title = Just s.title
          , tabs = Nothing
          , contents = s.form |> mdFormView
          , actions = Nothing
          }
            |> mdCardView
            |> mdRowView "s6 offset-s3"
        , div []
            (List.map mdLinkView s.links)
        ]


mdIconView : MDIcon -> Html Msg
mdIconView icon =
    i [ class ("material-icons " ++ icon.position) ] [ text icon.name ]


mdSideNavLayout : Html Msg -> Html Msg -> Html Msg -> Html Msg
mdSideNavLayout nav side main =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col s3" ]
                [ side ]
            , div [ class "col s9" ]
                [ main ]
            ]
        ]


mdLinkCollectionView : List MDLink -> Html Msg
mdLinkCollectionView links =
    div [ class "collection" ]
        (List.map mdCollectionItemView links)


mdCollectionItemView : MDLink -> Html Msg
mdCollectionItemView link =
    a [ href "#", class ("collection-item " ++ mdActiveClass link.active), onClick link.action ]
        [ text link.title ]


type alias MDIcon =
    { name : String
    , position : String
    }


type alias MDFormSection =
    { title : String
    , form : MDForm
    , links : List MDLink
    }


type alias MDCard =
    { title : Maybe String
    , tabs : Maybe (List MDLink)
    , contents : Html Msg
    , actions : Maybe (List MDLink)
    }


type alias MDLink =
    { title : String
    , active : Bool
    , action : Msg
    , icon : Maybe MDIcon
    }


type MDFieldClass
    = MDText
    | MDPasswd


type alias MDField =
    { class : MDFieldClass
    , title : String
    , id : String
    , icon : Maybe MDIcon
    , action : String -> Msg
    }


type alias MDFileField =
    { title : String
    , id : String
    , icon : Maybe MDIcon
    , action : List NativeFile -> Msg
    }


type alias MDForm =
    { fields : List MDField
    , files : List MDFileField
    , actions : List MDLink
    }


mdFormView : MDForm -> Html Msg
mdFormView form =
    div [ class "row" ]
        ((form.fields |> List.map mdFormFieldView)
            ++ (form.files |> List.map mdFileFieldView)
            ++ (form.actions |> List.map mdButtonView)
        )


mdFileFieldView : MDFileField -> Html Msg
mdFileFieldView f =
    div [ class "file-field input-field" ]
        [ div [ class "btn" ]
            [ span [] [ text f.title ]
            , input [ type_ "file", onFileChange f.action ] []
            ]
        , div [ class "file-path-wrapper" ]
            [ input [ class "file-path validate", type_ "text", placeholder f.title ] [] ]
        ]


mdFormFieldView : MDField -> Html Msg
mdFormFieldView f =
    case f.class of
        MDText ->
            mdTextField f

        MDPasswd ->
            mdPasswdField f


mdTextField : MDField -> Html Msg
mdTextField f =
    div [ class "input-field col s12" ]
        [ input [ id "", placeholder f.title, type_ "text", onInput f.action ] []
        , label [ for f.id, class "active" ] [ text f.title ]
        ]


mdPasswdField : MDField -> Html Msg
mdPasswdField f =
    div [ class "input-field col s12" ]
        [ input [ id "", placeholder f.title, type_ "password", onInput f.action ] []
        , label [ for f.id, class "active" ] [ text f.title ]
        ]


mdButtonView : MDLink -> Html Msg
mdButtonView l =
    let
        iconView =
            case l.icon of
                Nothing ->
                    []

                Just ic ->
                    [ ic |> mdIconView ]
    in
    div [ class "input-field col s12" ]
        [ a [ class "waves-effect waves-light btn col s12", onClick l.action ]
            (iconView ++ [ text l.title ])
        ]
