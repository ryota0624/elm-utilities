module EditableForm exposing (..)

import Html exposing (Html, Attribute)
import Html.Events exposing (onClick, onBlur, onInput)
import Task exposing (Task)
import Dom


type EditableForm value
    = InRead value
    | InEdit { prev : value, value : value }


type alias EditableTextForm =
    EditableForm String


initialEditableForm : String -> EditableTextForm
initialEditableForm text =
    InRead text


toggleEdit : EditableForm v -> EditableForm v
toggleEdit form =
    case form of
        InRead value ->
            InEdit { prev = value, value = value }

        _ ->
            form


resetInRead : EditableForm v -> EditableForm v
resetInRead form =
    case form of
        InEdit { prev, value } ->
            InRead prev

        _ ->
            form


updateEditForm : v -> EditableForm v -> EditableForm v
updateEditForm inputValue form =
    case form of
        InEdit { prev, value } ->
            InEdit { value = inputValue, prev = prev }

        InRead value ->
            form


save : EditableForm v -> ( EditableForm v, v )
save form =
    case form of
        InEdit { prev, value } ->
            ( InRead value, value )

        InRead value ->
            ( form, value )


type alias OnChangeFormPayload value =
    { form : EditableForm value
    , maybeTask : Maybe (Task Dom.Error ())
    }


type alias ViewEditingTextFormConfig msg value =
    { container : List (Attribute msg) -> List (Html msg) -> Html msg
    , containerAttributes : List (Attribute msg)
    , readView : value -> List (Attribute msg) -> Html msg
    , editView : value -> { cancel: msg, updateForm: (value -> msg), save: msg } -> List (Attribute msg) -> Html msg
    , onChangeForm : OnChangeFormPayload value -> msg
    , editFormUniqueId : String
    }


noTaskChangeForm : EditableForm v -> OnChangeFormPayload v
noTaskChangeForm form =
    { form = form, maybeTask = Nothing }


viewEditingForm : EditableForm String -> ViewEditingTextFormConfig msg String -> Html msg
viewEditingForm editingForm config =
    viewCustomEditingForm editingForm
        { container = config.container
        , containerAttributes = config.containerAttributes
        , onChangeForm = config.onChangeForm
        , readView = config.readView
        , editView = (\value -> \blur -> \updateEditForm -> config.editView value )
        , editFormUniqueId = config.editFormUniqueId
        }


type alias CustomViewConfig msg value =
    { container : List (Attribute msg) -> List (Html msg) -> Html msg
    , containerAttributes : List (Attribute msg)
    , readView : value -> List (Attribute msg) -> Html msg
    , editView :
        value
        -> msg
        -> (value -> msg)
        -> Html msg -- value, msg: cancel, value -> msg: onChangeForm
    , onChangeForm : OnChangeFormPayload value -> msg
    , editFormUniqueId : String
    }


viewCustomEditingForm : EditableForm v -> CustomViewConfig msg v -> Html msg
viewCustomEditingForm editingForm config =
    case editingForm of
        InRead value ->
            config.container ([ onClick <| config.onChangeForm <| { form = toggleEdit editingForm, maybeTask = Just (Dom.focus config.editFormUniqueId) } ] ++ config.containerAttributes)
                [ config.readView value []
                ]

        InEdit { prev, value } ->
            config.container config.containerAttributes
                [ config.editView value (config.onChangeForm <| noTaskChangeForm <| resetInRead editingForm) ((flip updateEditForm) editingForm >> noTaskChangeForm >> config.onChangeForm)
                ]
