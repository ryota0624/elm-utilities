module Main exposing (..)

import Dom
import Html exposing (Html, text, div, h1, img, input, span)
import Html.Attributes as Attr exposing (src, value, id)
import EditableElement
import Dict exposing (Dict)
import Task exposing (Task)
import Html.Events as Events
import LabeledGroupCheckBox
import SelectForm
import TableFrame
import SuggestableSelector as SS


---- MODEL ----


type alias Id =
    String


type alias Model =
    { users : Dict Id UserFormDto
    , selector : SS.Selector
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
    , name : EditableElement.EditableElement String
    , age : EditableElement.EditableElement String
    , gender : EditableElement.EditableElement Gender
    , skills : EditableElement.EditableElement (List Skill)
    , user : User
    }


type UserFormMsg
    = UpdateName (EditableElement.EditableElement String)
    | UpdateAge (EditableElement.EditableElement String)
    | UpdateGender (EditableElement.EditableElement Gender)
    | UpdateSkills (EditableElement.EditableElement (List Skill))


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
    , name = EditableElement.editableElement user.name
    , age = EditableElement.editableElement <| toString <| user.age
    , gender = EditableElement.editableElement user.gender
    , skills = EditableElement.editableElement user.skills
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
    ( { selector =
            { status = SS.Close
            }
      , users = users |> List.map (\user -> ( user.id, userToDto user )) |> Dict.fromList
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateUserDto Id EditableElement.Event UserFormMsg
    | ReceiveFocusResult (Result Dom.Error ())
    | UpdateSelector SS.Selector
    | Log String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
         UpdateUserDto id formEvent userMsg ->
             let
                cmd = case formEvent of
                    EditableElement.Focus task ->
                        Task.attempt ReceiveFocusResult task
                    EditableElement.Save ->
                        Debug.crash ("save")
                    EditableElement.Cancel ->
                        Cmd.none
                    EditableElement.None ->
                        Cmd.none

                updatedUsers =
                   model.users |> Dict.update id (Maybe.map (updateUserFormDto userMsg))
             in
                ( { model | users = updatedUsers }, cmd)
         ReceiveFocusResult result ->
            ( model, Cmd.none )

         UpdateSelector selector ->
            ( { model | selector = selector }, Cmd.none )

         Log str ->
            (model , Cmd.none) |> Debug.log str


---- VIEW ----


conf : SS.SelectorConfig String Msg
conf =
    { selectorLabel = "SelectorLabel"
    , notFoundMatchedValueLabel = "一致なし"
    , placeholder = "入力してね"
    , optionValues = [ "App", "Apple", "Orange" ]
    , optionValueToHtml = \value -> Html.li [ Events.onClick <| Log value ] [ Html.text value ]
    , findSelectorOptionValue = \text -> \value -> String.contains text value
    , onEvent = UpdateSelector
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ SS.view conf model.selector

                , viewUserTable users
                , Html.ul [] (model.users |> Dict.values |> List.map userEditableElement)
        ]


userEditableElement : UserFormDto -> Html Msg
userEditableElement user =
    Html.li []
        [ text "name: "
        , EditableElement.viewEditableElement user.name
            { displayView =
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
            , onChange = (\event -> UpdateName >> UpdateUserDto user.id event)
            , uniqueId = "username"
            }
        , text ",   age: "
        , EditableElement.viewEditableElement user.age
            { displayView =
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
            , onChange = (\event -> UpdateAge >> UpdateUserDto user.id event)
            , uniqueId = "age"
            }
        , text ",   gender: "
        , EditableElement.viewEditableElement user.gender
            { displayView =
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
            , onChange = (\event -> UpdateGender >> UpdateUserDto user.id event)
            , uniqueId = "gender"
            }
        , text ",   skills: "
        , EditableElement.viewEditableElement user.skills
            { displayView =
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
            , onChange = (\event -> UpdateSkills >> UpdateUserDto user.id event)
            , uniqueId = "skills"
            }
        ]


type UserTableHeader
    = ID
    | NAME
    | AGE
    | GENDER
    | SKILLS


viewUserTable : List User -> Html msg
viewUserTable users =
    TableFrame.view
        { headerElements = [ NAME, AGE, GENDER, SKILLS, ID ]
        , datas = users
        , viewTableContainer = Html.table []
        , viewBodyContainer = Html.tbody []
        , viewHeaderContainer = Html.thead []
        , headerConfig =
            { viewHeaderRowContainer = Html.tr []
            , viewHeaderRowCell = \element -> Html.th [] [ Html.text <| toString element ]
            }
        , rowConfig =
            { viewTableRowContainer = \data -> Html.tr []
            , viewTableRowCell =
                \data ->
                    \headerElement ->
                        let
                            container element =
                                Html.td [] [ Html.text element ]
                        in
                            container <|
                                case headerElement of
                                    ID ->
                                        data.id |> toString

                                    NAME ->
                                        data.name

                                    AGE ->
                                        data.age |> toString

                                    GENDER ->
                                        data.gender |> toString

                                    SKILLS ->
                                        data.skills |> toString
            }
        }



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
