module SuggestableSelector exposing (Selector, SelectorConfig, view, Status(..))

import Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type Status
    = Open String
    | Close


type alias Selector =
    { status : Status
    }


type alias FindSelectorOptionValue selectorOptionValue =
    String -> selectorOptionValue -> Bool


type alias SelectorConfig selectorOptionValue msg =
    { selectorLabel : String
    , notFoundMatchedValueLabel : String
    , placeholder : String
    , optionValues : List selectorOptionValue
    , optionValueToHtml : selectorOptionValue -> Html msg
    , findSelectorOptionValue : FindSelectorOptionValue selectorOptionValue
    , onEvent : Selector -> msg
    }


openSelector : Selector -> Selector
openSelector selector =
    { selector | status = Open "" }


closeSelector : Selector -> Selector
closeSelector selector =
    { selector | status = Close }


inputText : Selector -> String -> Selector
inputText selector text =
    { selector | status = Open text }


getInputText : Selector -> String
getInputText selector =
    case selector.status of
        Open text ->
            text

        Close ->
            ""


viewSelectorTextInput : SelectorConfig selectorOptionValue msg -> Selector -> Html msg
viewSelectorTextInput config selector =
    case selector.status of
        Close ->
            Html.p [ Events.onClick (config.onEvent (openSelector selector)) ] [ Html.text config.selectorLabel ]

        Open inputtingString ->
            Html.input
                [ Attr.value inputtingString
                , Attr.placeholder config.placeholder
                , Events.onInput (inputText selector >> config.onEvent)
                ]
                []


viewSelectorOptions : SelectorConfig selectorOptionValue msg -> Selector -> List (Html msg)
viewSelectorOptions config selector =
    config.optionValues
        |> List.filter (getInputText selector |> config.findSelectorOptionValue)
        |> List.map config.optionValueToHtml


view : SelectorConfig selectorOptionValue msg -> Selector -> Html msg
view config selector =
    let
        styleSelectorOutRange =
            case selector.status of
                Open _ ->
                    Attr.style
                        [ ( "position", "fixed" )
                        , ( "width", "100%" )
                        , ( "height", "100%" )
                        , ( "zIndex", "100" )
                        , ( "top", "0px" )
                        , ( "left", "0px" )
                        ]

                Close ->
                    Attr.style [ ( "display", "none" ) ]

        styleSelectorContainer =
            case selector.status of
                Open _ ->
                    Attr.style
                        [ ( "position", "relative" )
                        , ( "zIndex", "101" )
                        ]

                Close ->
                    Attr.style []
    in
        Html.div []
            [ Html.div [ styleSelectorContainer ]
                [ viewSelectorTextInput config selector
                , case selector.status of
                    Close ->
                        Html.text ""

                    Open inputtingString ->
                        Html.ul
                            []
                            (viewSelectorOptions config selector)
                ]
            , Html.div
                [ Events.onClick <| config.onEvent (closeSelector selector)
                , styleSelectorOutRange
                ]
                []
            ]
