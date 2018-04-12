module Main exposing (..)

import Dom
import Html exposing (Html, text, div, h1, img, input, span)
import Html.Attributes as Attr exposing (src, value, id)
import EditableForm
import Dict exposing (Dict)
import Task exposing (Task)
import Html.Events as Events
import LabeledGroupCheckBox
import SelectForm

---- MODEL ----


type alias Id =
    String


type alias Model =
    { users : Dict Id UserFormDto
    }


type Gender
    = Men
    | Women


type Skill
    = HOGE
    | HUGA
    | FOOO


type alias UserFormDto =
    { id : String
    , name : EditableForm.EditableTextForm
    , age : EditableForm.EditableTextForm
    , gender : EditableForm.EditableForm Gender
    , skills : EditableForm.EditableForm (List Skill)
    , user : User
    }


type UserFormMsg
    = UpdateName EditableForm.EditableTextForm
    | UpdateAge EditableForm.EditableTextForm
    | UpdateGender (EditableForm.EditableForm Gender)
    | UpdateSkills (EditableForm.EditableForm (List Skill))


updateUserFormDto : UserFormMsg -> UserFormDto -> UserFormDto
updateUserFormDto msg dto =
    case msg of
        UpdateName form ->
            { dto | name = form }

        UpdateAge form ->
            { dto | age = form }

        UpdateGender form ->
            { dto | gender = form }

        UpdateSkills form ->
            { dto | skills = form }


type alias User =
    { id : Id
    , name : String
    , age : Int
    , gender : Gender
    , skills : List Skill
    }


userToDto : User -> UserFormDto
userToDto user =
    { id = user.id
    , name = EditableForm.initialEditableForm user.name
    , age = EditableForm.initialEditableForm <| toString <| user.age
    , gender = EditableForm.initialEditableForm user.gender
    , skills = EditableForm.initialEditableForm user.skills
    , user = user
    }


users : List User
users =
    [ User "id1" "suzuki" 24 Men [ HUGA ]
    , User "id2" "sasaki" 26 Women [ HUGA, HOGE ]
    , User "id3" "tanaka" 30 Men []
    ]


init : ( Model, Cmd Msg )
init =
    ( { users = users |> List.map (\user -> ( user.id, userToDto user )) |> Dict.fromList }, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateUserDto Id UserFormMsg
    | ReceiveFocusResult (Result Dom.Error ())
    | SaveForm Id UserFormMsg
    | FocusForm Id UserFormMsg (Task Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusForm id userMsg task ->
            let
                cmd =
                    Task.attempt ReceiveFocusResult task

                ( nextModel, subCmd ) =
                    update (UpdateUserDto id userMsg) model
            in
                ( nextModel, Cmd.batch [ cmd, subCmd ] )

        UpdateUserDto id userMsg ->
            let
                updatedUsers =
                    model.users |> Dict.update id (Maybe.map (updateUserFormDto userMsg))
            in
                ( { model | users = updatedUsers }, Cmd.none )

        SaveForm id userMsg ->
            let
                ( nextModel, subCmd ) =
                    update (UpdateUserDto id userMsg) model
            in
                ( nextModel, Cmd.batch [ Cmd.none, subCmd ] )

        ReceiveFocusResult result ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.ul [] (model.users |> Dict.values |> List.map userEditableForm)


userEditableForm : UserFormDto -> Html Msg
userEditableForm user =
    Html.li []
        [ text "name: "
        , EditableForm.viewEditingForm user.name
            { readView =
                \username -> \{ focus } -> Html.span [ Events.onClick focus ] [ text username ]
            , editView =
                \username ->
                    \{ cancel, update, save } ->
                        div []
                            [ div [ Attr.class "editable-input-area" ]
                                [ input [ id "username", value username, Events.onInput update ] []
                                , Html.button [ Events.onClick save ] [ text "保存" ]
                                ]
                            , div [ Attr.class "editable-close-area", Events.onClick cancel ] []
                            ]
            , onChangeForm = (UpdateName >> (UpdateUserDto user.id))
            , onFocus = (\form -> \task -> (FocusForm user.id (UpdateName form) task))
            , onSave = (\form -> (SaveForm user.id (UpdateName form)))
            , editFormUniqueId = "username"
            }
        , text ",   age: "
        , EditableForm.viewEditingForm user.age
            { readView =
                \v -> \{ focus } -> Html.span [ Events.onClick focus ] [ text v ]
            , editView =
                \v ->
                    \{ cancel, update, save } ->
                        div []
                            [ div [ Attr.class "editable-input-area" ]
                                [ input [ id "age", value v, Events.onInput update ] []
                                , Html.button [ Events.onClick save ] [ text "保存" ]
                                ]
                            , div [ Attr.class "editable-close-area", Events.onClick cancel ] []
                            ]
            , onChangeForm = (UpdateAge >> (UpdateUserDto user.id))
            , onFocus = (\form -> \task -> (FocusForm user.id (UpdateAge form) task))
            , onSave = (\form -> (SaveForm user.id (UpdateAge form)))
            , editFormUniqueId = "age"
            }
        , text ",   gender: "
        , EditableForm.viewEditingForm user.gender
            { readView =
                \v -> \{ focus } -> Html.span [ Events.onClick focus ] [ text <| toString <| v ]
            , editView =
                \v ->
                    \{ cancel, update, save } ->
                        let
                            selectorConfig =
                                SelectForm.defaultSelectConfig v update
                        in
                            div []
                                [ div [ Attr.class "editable-input-area" ]
                                    [ SelectForm.selectForm selectorConfig [ Men, Women ]
                                    , Html.button [ Events.onClick save ] [ text "保存" ]
                                    ]
                                , div [ Attr.class "editable-close-area", Events.onClick cancel ] []
                                ]
            , onChangeForm = (\form -> (UpdateUserDto user.id (UpdateGender form)))
            , onFocus = (\form -> \task -> (FocusForm user.id (UpdateGender form) task))
            , onSave = (\form -> (SaveForm user.id (UpdateGender form)))
            , editFormUniqueId = "gender"
            }
        , text ",   skills: "
        , EditableForm.viewEditingForm user.skills
            { readView =
                \v -> \{ focus } -> Html.span [ Events.onClick focus ] [ text <| toString <| v ]
            , editView =
                \v ->
                    \{ cancel, update, save } ->
                        let
                            selectorConfig =
                                SelectForm.defaultSelectConfig v update
                        in
                            div []
                                [ div [ Attr.class "editable-input-area" ]
                                    [ LabeledGroupCheckBox.labeledCheckBoxGroup
                                        { uniqueId = (toString >> (++) "-user-skill")
                                        , container = div []
                                        , onChangeChecked = update
                                        , labelContainer = \_ -> div []
                                        , toLabelString = toString
                                        , toValueString = toString
                                        }
                                        [ HOGE, HUGA, FOOO ]
                                        v
                                    , Html.button [ Events.onClick save ] [ text "保存" ]
                                    ]
                                , div [ Attr.class "editable-close-area", Events.onClick cancel ] []
                                ]
            , onChangeForm = (\form -> (UpdateUserDto user.id (UpdateSkills form)))
            , onFocus = (\form -> \task -> (FocusForm user.id (UpdateSkills form) task))
            , onSave = (\form -> (SaveForm user.id (UpdateSkills form)))
            , editFormUniqueId = "skills"
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
