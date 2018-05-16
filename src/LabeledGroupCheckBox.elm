module LabeledGroupCheckBox exposing (..)

{-|


# Definition

@docs LabeledGroupCheckBoxConfig


# function

@docs labeledCheckBoxGroup

-}

import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events


{-| -}
type alias LabeledGroupCheckBoxConfig a msg =
    { uniqueId : a -> String
    , onChangeChecked : List a -> msg
    , container : List (Html.Html msg) -> Html.Html msg
    , labelContainer : a -> List (Html.Html msg) -> Html.Html msg
    , toLabelString : a -> String
    , toValueString : a -> String
    }


{-| -}
labeledCheckBoxGroup : LabeledGroupCheckBoxConfig a msg -> List a -> List a -> Html msg
labeledCheckBoxGroup config selectableValues selectedValues =
    config.container
        (selectableValues
            |> List.map
                (\value ->
                    let
                        isBoxChecked =
                            selectedValues |> List.member value

                        onCheckedHandleValue =
                            if isBoxChecked then
                                selectedValues |> List.filter (\v -> not (v == value))
                            else
                                selectedValues ++ [ value ]
                    in
                        config.container
                            [ Html.label [ Attr.for <| config.uniqueId value ] [ text <| config.toLabelString <| value ]
                            , Html.input
                                [ Attr.type_ "checkbox"
                                , Attr.id <| config.uniqueId value
                                , Attr.checked isBoxChecked
                                , Attr.value <| config.toValueString <| value
                                , Events.onClick <| config.onChangeChecked onCheckedHandleValue
                                ]
                                []
                            ]
                )
        )
