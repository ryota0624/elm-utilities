module ScheduleSelector exposing (..)

import Date
import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


dayToInt : Date.Day -> Int
dayToInt day =
    case day of
        Date.Mon ->
            1

        Date.Tue ->
            2

        Date.Wed ->
            3

        Date.Thu ->
            4

        Date.Fri ->
            5

        Date.Sat ->
            6

        Date.Sun ->
            7


type alias Hour =
    Int


hours : List Int
hours =
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 ]


type alias HoursSetting =
    Dict.Dict Hour Bool


flipHoursSetting : Hour -> HoursSetting -> HoursSetting
flipHoursSetting hour =
    Dict.update hour (Maybe.map (not))


updateDaySetting : DaySetting -> ScheduleSelector -> ScheduleSelector
updateDaySetting daySetting scheduleSelector =
    let
        daySettings =
            scheduleSelector.daySettings
                |> List.map
                    (\daySetting_ ->
                        if daySetting.day == daySetting_.day then
                            daySetting
                        else
                            daySetting_
                    )
    in
        { scheduleSelector | daySettings = daySettings }


type alias DaySetting =
    { day : Date.Day
    , hoursSetting : HoursSetting
    }


allDisabledHoursSetting : HoursSetting
allDisabledHoursSetting =
    hours
        |> List.map (\hour -> ( hour, False ))
        |> Dict.fromList


allEnableHoursSetting : HoursSetting
allEnableHoursSetting =
    hours
        |> List.map (\hour -> ( hour, True ))
        |> Dict.fromList


type MouseStatus
    = Down
        { mouseDownStartDay : Date.Day
        , mouseDownStartHour : Int
        , daySettingsSnapShot : List DaySetting
        }
    | Up


type alias ScheduleSelector =
    { daySettings : List DaySetting
    , disabled : Bool
    , mouseStatus : MouseStatus
    }


isMouseDown : ScheduleSelector -> Bool
isMouseDown scheduleSelector =
    case scheduleSelector.mouseStatus of
        Down _ ->
            True

        Up ->
            False


isMouseUp : ScheduleSelector -> Bool
isMouseUp =
    isMouseDown >> not


emptyScheduleSelector : Bool -> ScheduleSelector
emptyScheduleSelector disabled =
    { daySettings =
        [ { hoursSetting = allDisabledHoursSetting, day = Date.Mon }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Tue }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Wed }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Thu }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Fri }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Sat }
        , { hoursSetting = allDisabledHoursSetting, day = Date.Sun }
        ]
    , disabled = disabled
    , mouseStatus = Up
    }


allNonSelected : ScheduleSelector -> ScheduleSelector
allNonSelected selector =
    { selector
        | daySettings = (emptyScheduleSelector selector.disabled).daySettings
    }


allSelected : ScheduleSelector -> ScheduleSelector
allSelected selector =
    { selector
        | daySettings =
            [ { hoursSetting = allEnableHoursSetting, day = Date.Mon }
            , { hoursSetting = allEnableHoursSetting, day = Date.Tue }
            , { hoursSetting = allEnableHoursSetting, day = Date.Wed }
            , { hoursSetting = allEnableHoursSetting, day = Date.Thu }
            , { hoursSetting = allEnableHoursSetting, day = Date.Fri }
            , { hoursSetting = allEnableHoursSetting, day = Date.Sat }
            , { hoursSetting = allEnableHoursSetting, day = Date.Sun }
            ]
    }


click : Hour -> Date.Day -> ScheduleSelector -> ScheduleSelector
click hour day selector =
    let
        updatedDaySettings =
            selector.daySettings
                |> List.map
                    (\daySetting ->
                        if (daySetting.day == day) then
                            let
                                updatedHoursSetting =
                                    daySetting.hoursSetting
                                        |> flipHoursSetting hour
                            in
                                { daySetting | hoursSetting = updatedHoursSetting }
                        else
                            daySetting
                    )
    in
        { selector | daySettings = updatedDaySettings }


mouseDown : Hour -> DaySetting -> ScheduleSelector -> ScheduleSelector
mouseDown hour daySetting selector =
    let
        updatedHoursSetting =
            daySetting.hoursSetting
                |> flipHoursSetting hour

        updatedDaySetting =
            { daySetting
                | hoursSetting = updatedHoursSetting
            }

        updatedSelector =
            updateDaySetting updatedDaySetting selector
    in
        { updatedSelector
            | mouseStatus =
                Down
                    { mouseDownStartHour = hour
                    , mouseDownStartDay = daySetting.day
                    , daySettingsSnapShot = selector.daySettings
                    }
        }


mouseUp : ScheduleSelector -> ScheduleSelector
mouseUp selector =
    case selector.mouseStatus of
        Up ->
            selector

        Down { daySettingsSnapShot } ->
            { selector
                | mouseStatus = Up
                , daySettings = selector.daySettings
            }


isInRangeInt : Int -> Int -> Int -> Bool
isInRangeInt left right targetInt =
    left <= targetInt && right >= targetInt


mouseEnter : Hour -> Date.Day -> ScheduleSelector -> ScheduleSelector
mouseEnter mouseEnterHour mouseEnterDay selector =
    case selector.mouseStatus of
        Down { mouseDownStartHour, mouseDownStartDay, daySettingsSnapShot } ->
            let
                ( startIntDay, endIntDay ) =
                    ( min (dayToInt mouseEnterDay) (dayToInt mouseDownStartDay)
                    , max (dayToInt mouseEnterDay) (dayToInt mouseDownStartDay)
                    )

                ( startHour, endHour ) =
                    ( min mouseDownStartHour mouseEnterHour
                    , max mouseDownStartHour mouseEnterHour
                    )

                isInRangeFlipTargetDay day =
                    isInRangeInt startIntDay endIntDay (dayToInt day)

                isInRangeFlipTargetHour hour =
                    isInRangeInt startHour endHour hour

                updatedDaySettings =
                    daySettingsSnapShot
                        |> List.map
                            (\daySetting ->
                                if (isInRangeFlipTargetDay daySetting.day) then
                                    let
                                        updatedHoursSetting =
                                            daySetting.hoursSetting
                                                |> Dict.map
                                                    (\hour ->
                                                        \isSelected ->
                                                            if (isInRangeFlipTargetHour hour) then
                                                                not isSelected
                                                            else
                                                                isSelected
                                                    )
                                    in
                                        { daySetting | hoursSetting = updatedHoursSetting }
                                else
                                    daySetting
                            )
            in
                { selector | daySettings = updatedDaySettings }

        Up ->
            selector


viewHoursLine : Html.Html msg
viewHoursLine =
    Html.thead []
        [ Html.tr []
            ([ Html.tr [ Attr.class "sch-cell" ] [] ]
                ++ (hours
                        |> List.map
                            (\hour ->
                                Html.th [ Attr.class "sch-cell" ] [ Html.text (toString hour) ]
                            )
                   )
            )
        ]


type alias ViewHourCellConfig msg =
    { onMouseDown : Hour -> msg
    , onMouseUp : msg
    , onMouseEnter : Hour -> msg
    , onClick : Hour -> msg
    }


viewHourCell : ScheduleSelector -> ViewHourCellConfig msg -> Hour -> Bool -> Html msg
viewHourCell selector config hour isSelected =
    let
        classNames =
            if isSelected then
                Attr.class "sch-selected"
            else
                Attr.class "sch-selectable"

        eventHandlers =
            if (isMouseDown selector) then
                [ Events.onMouseUp config.onMouseUp
                , Events.onMouseEnter (config.onMouseEnter hour)
                ]
            else
                [ Events.onMouseDown (config.onMouseDown hour)
                , Events.onClick (config.onClick hour)
                ]
    in
        Html.td
            ([ classNames
             ]
                ++ eventHandlers
            )
            [ toString hour |> Html.text ]


type alias ViewDayPanelConfig msg =
    { onMouseDown : Hour -> DaySetting -> msg
    , onMouseUp : msg
    , onMouseEnter : Hour -> Date.Day -> msg
    , onClick : Hour -> Date.Day -> msg
    }


viewDayLinePanel : ScheduleSelector -> ViewDayPanelConfig msg -> DaySetting -> Html.Html msg
viewDayLinePanel selector config daySetting =
    let
        viewHourCellConfig =
            { onMouseDown = \hour -> config.onMouseDown hour daySetting
            , onMouseUp = config.onMouseUp
            , onMouseEnter = \hour -> config.onMouseEnter hour daySetting.day
            , onClick = \hour -> config.onClick hour daySetting.day
            }
    in
        Html.tr []
            ([ Html.td [] [ Html.text <| toString daySetting.day ] ]
                ++ (daySetting.hoursSetting
                        |> Dict.map (viewHourCell selector viewHourCellConfig)
                        |> Dict.values
                   )
            )


view : (ScheduleSelector -> msg) -> ScheduleSelector -> Html.Html msg
view eventHandler selector =
    let
        handleMouseDown hour daySetting =
            mouseDown hour daySetting selector |> eventHandler

        handleMouseEnter mouseEnterHour mouseEnterDay =
            mouseEnter mouseEnterHour mouseEnterDay selector |> eventHandler

        handleMouseUp =
            mouseUp selector |> eventHandler

        handleClick hour day =
            click hour day selector |> eventHandler

        handleLeaveTableBody =
            { selector | mouseStatus = Up } |> eventHandler
    in
        Html.table
            [ Events.onMouseLeave handleLeaveTableBody
            , Attr.class "sch-container sch-table"
            ]
            ([ viewHoursLine
             ]
                ++ (selector.daySettings
                        |> List.map
                            (viewDayLinePanel selector
                                { onMouseDown = handleMouseDown
                                , onMouseUp = handleMouseUp
                                , onMouseEnter = handleMouseEnter
                                , onClick = handleClick
                                }
                            )
                   )
            )
