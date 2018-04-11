module Main exposing (..)

import Dom
import Html exposing (Html, text, div, h1, img, input, span)
import Html.Attributes exposing (src, value, id)
import EditableForm
import Task
import Dict exposing (Dict)


---- MODEL ----


type alias Id =
    String


type alias Model =
    { users : Dict Id UserFormDto
    }


type alias UserFormDto =
    { id : String
    , name : EditableForm.EditableTextForm
    , age : EditableForm.EditableTextForm
    }


type alias User =
    { id : Id
    , name : String
    , age : Int
    }


userToDto : User -> UserFormDto
userToDto user =
    { id = user.id
    , name = EditableForm.initialEditableForm user.name
    , age = EditableForm.initialEditableForm <| toString <| user.age
    }


users =
    [ User "id1" "suzuki" 24
    , User "id2" "sasaki" 26
    , User "id3" "tanaka" 30
    ]


init : ( Model, Cmd Msg )
init =
    ( { users = users |> List.map (\user -> ( user.id, userToDto user )) |> Dict.fromList }, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateEditableForm Id EditableField (EditableForm.OnChangeFormPayload String)
    | ReceiveFocusResult (Result Dom.Error ())


type EditableField
    = Name
    | Age


updateUserFormDto : EditableField -> EditableForm.EditableTextForm -> UserFormDto -> UserFormDto
updateUserFormDto field form user =
    case field of
        Name ->
            { user | name = form }

        Age ->
            { user | age = form }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEditableForm id field { form, maybeTask } ->
            let
                cmd =
                    maybeTask |> Maybe.map (Task.attempt ReceiveFocusResult) |> Maybe.withDefault (Cmd.none)

                updatedUsers =
                    model.users |> Dict.update id (Maybe.map (updateUserFormDto field form))
            in
                ( { model | users = updatedUsers }, cmd )

        ReceiveFocusResult result ->
            let
                a =
                    Debug.log "msg" msg
            in
                Debug.log "" ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
        Html.ul [] (model.users |> Dict.values |> List.map userEditableForm)



userEditableForm : UserFormDto -> Html Msg
userEditableForm user =
    Html.li []
        [ text "name: "
        , EditableForm.viewEditingForm user.name
            { readView = \v -> \attrs -> text v
            , editView = \v -> \attrs -> input ([ id "username", value v ] ++ attrs) []
            , container = span
            , containerAttributes = []
            , onChangeForm = UpdateEditableForm user.id Name
            , editFormUniqueId = "username"
            }
        , text "age: "
        , EditableForm.viewEditingForm user.age
            { readView = \v -> \attrs -> text v
            , editView = \v -> \attrs -> input ([ id "userage", value v ] ++ attrs) []
            , container = span
            , containerAttributes = []
            , onChangeForm = UpdateEditableForm user.id Age
            , editFormUniqueId = "userage"
            }
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
