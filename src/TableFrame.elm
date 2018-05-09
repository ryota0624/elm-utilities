module TableFrame exposing (..)

import Html exposing (Html)


type alias HeaderConfig headerElement msg =
    { viewHeaderRowContainer : List (Html msg) -> Html msg
    , viewHeaderRowCell : headerElement -> Html msg
    }


viewTableHeader : List headerElement -> HeaderConfig headerElement msg -> Html msg
viewTableHeader headerElements config =
    config.viewHeaderRowContainer (headerElements |> List.map config.viewHeaderRowCell)


type alias RowConfig headerElement msg data =
    { viewTableRowContainer : data -> List (Html msg) -> Html msg
    , viewTableRowCell : data -> headerElement -> Html msg
    }


viewTableRow : RowConfig headerElement msg data -> List headerElement -> data -> Html msg
viewTableRow config headerElements data =
    config.viewTableRowContainer data (headerElements |> List.map (config.viewTableRowCell data))


type alias Config headerElement msg data =
    { datas : List data
    , headerElements : List headerElement
    , viewTableContainer : List (Html msg) -> Html msg
    , viewHeaderContainer : List (Html msg) -> Html msg
    , viewBodyContainer : List (Html msg) -> Html msg
    , headerConfig : HeaderConfig headerElement msg
    , rowConfig : RowConfig headerElement msg data
    }


view : Config headerElement msg data -> Html msg
view config =
    config.viewTableContainer
        [ config.viewHeaderContainer [ config.headerConfig |> viewTableHeader config.headerElements ]
        , config.viewBodyContainer (config.datas |> List.map (viewTableRow config.rowConfig config.headerElements))
        ]
