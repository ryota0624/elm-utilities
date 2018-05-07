module EditableElement exposing (EditableElement, editableElement, ViewConfig, EditViewProps, viewEditableElement, Event(..))

{-|
# Definition
@docs EditableElement, EditViewProps, ViewConfig, Event

# function
@docs editableElement, viewEditableElement

-}

import Html exposing (Html, Attribute)
import Task exposing (Task)
import Dom
{-|
-}
type Event = Save | Focus (Task Dom.Error ()) | Cancel | None

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
    , onChange : Event -> EditableElement value -> msg
    , uniqueId : String
    }

{-| 
-}
viewEditableElement : EditableElement v -> ViewConfig msg v -> Html msg
viewEditableElement element config =
    case element of
        Displaying value ->
            config.displayView value { focus = config.onChange (Focus <| Dom.focus config.uniqueId) (toggleToEditing element) }

        Editing { prev, value } ->
            config.editView value
                { cancel = config.onChange Cancel (resetToDisplaying element)
                , update = (flip update) element >> (\updatedForm -> config.onChange None updatedForm)
                , save = config.onChange Save (save element)
                }
