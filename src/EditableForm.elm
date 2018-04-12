module EditableForm exposing (EditableForm, EditableTextForm, initialEditableForm, ViewConfig, EditViewProps, viewEditingForm)

{-|
# Definition
@docs EditableForm, EditableTextForm, EditViewProps, ViewConfig

# function
@docs initialEditableForm, viewEditingForm

-}

import Html exposing (Html, Attribute)
import Task exposing (Task)
import Dom

{-|
-}
type EditableForm value
    = InRead value
    | InEdit { prev : value, value : value }


{-|
-}
type alias EditableTextForm =
    EditableForm String


{-|
-}
initialEditableForm : a -> EditableForm a
initialEditableForm a =
    InRead a


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


save : EditableForm v -> EditableForm v
save form =
    case form of
        InEdit { prev, value } ->
            InRead value

        InRead value ->
            form

{-|
-}
type alias EditViewProps msg value =
    { cancel : msg
    , update : value -> msg
    , save : msg
    }


{-|
-}
type alias ViewConfig msg value =
    { readView : value -> { focus : msg } -> Html msg
    , editView :
        value
        -> EditViewProps msg value
        -> Html msg
    , onFocus : EditableForm value -> Task Dom.Error () -> msg
    , onSave : EditableForm value -> msg
    , onChangeForm : EditableForm value -> msg
    , editFormUniqueId : String
    }

{-| 
-}
viewEditingForm : EditableForm v -> ViewConfig msg v -> Html msg
viewEditingForm editingForm config =
    case editingForm of
        InRead value ->
            config.readView value { focus = config.onFocus (toggleEdit editingForm) (Dom.focus config.editFormUniqueId) }

        InEdit { prev, value } ->
            config.editView value
                { cancel = (config.onChangeForm <| resetInRead editingForm)
                , update = ((flip updateEditForm) editingForm >> config.onChangeForm)
                , save = (config.onSave <| save editingForm)
                }
