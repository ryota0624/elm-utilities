module EditableElement exposing (EditableElement, editableElement, ViewConfig, EditViewProps, viewEditableElement)

{-|
# Definition
@docs EditableElement, EditableTextForm, EditViewProps, ViewConfig

# function
@docs editableElement, viewEditingForm

-}

import Html exposing (Html, Attribute)
import Task exposing (Task)
import Dom

{-|
-}
type EditableElement value
    = Displaying value
    | Editing { prev : value, value : value }

{-|
-}
editableElement : a -> EditableElement a
editableElement a =
    Displaying a


toggleToEditing : EditableElement v -> EditableElement v
toggleToEditing form =
    case form of
        Displaying value ->
            Editing { prev = value, value = value }

        _ ->
            form


resetToDisplaying : EditableElement v -> EditableElement v
resetToDisplaying form =
    case form of
        Editing { prev, value } ->
            Displaying prev

        _ ->
            form


update : v -> EditableElement v -> EditableElement v
update inputValue element =
    case element of
        Editing { prev, value } ->
            Editing { value = inputValue, prev = prev }

        Displaying value ->
            element


save : EditableElement v -> EditableElement v
save form =
    case form of
        Editing { prev, value } ->
            Displaying value

        Displaying value ->
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
    { displayView : value -> { focus : msg } -> Html msg
    , editView :
        value
        -> EditViewProps msg value
        -> Html msg
    , onFocus : EditableElement value -> Task Dom.Error () -> msg
    , onSave : EditableElement value -> msg
    , onChange : EditableElement value -> msg
    , uniqueId : String
    }

{-| 
-}
viewEditableElement : EditableElement v -> ViewConfig msg v -> Html msg
viewEditableElement element config =
    case element of
        Displaying value ->
            config.displayView value { focus = config.onFocus (toggleToEditing element) (Dom.focus config.uniqueId) }

        Editing { prev, value } ->
            config.editView value
                { cancel = (config.onChange <| resetToDisplaying element)
                , update = ((flip update) element >> config.onChange)
                , save = (config.onSave <| save element)
                }
