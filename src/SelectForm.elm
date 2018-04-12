module SelectForm exposing (..)

{-|
# Definition
@docs SelectFormConfig

# function
@docs defaultSelectConfig, selectForm

-}
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events

{-|
-}
type alias SelectFormConfig a msg =
    { optionAttrs : a -> List (Html.Attribute msg)
    , attrs : List (Html.Attribute msg)
    , toString : a -> String
    , toLabel : a -> String
    , value : a
    , onSelect : a -> msg
    }

{-|
-}
defaultSelectConfig : a -> (a -> msg) -> SelectFormConfig a msg
defaultSelectConfig value onSelect =
    { optionAttrs = \_ -> []
    , attrs = []
    , toString = toString
    , toLabel = toString
    , value = value
    , onSelect = onSelect
    }

{-|
-}
selectForm : SelectFormConfig a msg -> List a -> Html msg
selectForm config optionItems =
    let
        onInputHandler : String -> a
        onInputHandler str =
            optionItems
                |> List.filter (\value -> str == (config.toString value))
                |> List.head
                |> (\maybe ->
                        case maybe of
                            Just value ->
                                value

                            Nothing ->
                                Debug.crash ("unknown selected value:" ++ str)
                   )
    in
        Html.select (config.attrs ++ [ Attr.value <| config.toString <| config.value, Events.onInput (onInputHandler >> config.onSelect) ])
            (optionItems
                |> List.map
                    (\item ->
                        Html.option
                            ([ Attr.value <| config.toString <| item
                             ]
                                ++ config.optionAttrs item
                            )
                            [ text <| config.toLabel <| item ]
                    )
            )
