module Ui.WithContext.Layout exposing
    ( Width
    , bounded
    , byContent
    , fill
    , portion
    , px
    , rowWithConstraints
    )

import Ui.Layout
import Ui.WithContext as Ui


type alias Width =
    Ui.Layout.Width


fill : Width
fill =
    Ui.Layout.fill


portion : Int -> Width
portion =
    Ui.Layout.portion


px : Int -> Width
px =
    Ui.Layout.px


bounded : { min : Maybe Int, max : Maybe Int } -> Width
bounded =
    Ui.Layout.bounded


byContent : Width
byContent =
    Ui.Layout.byContent


rowWithConstraints : List Ui.Layout.Width -> List (Ui.Attribute context msg) -> List (Ui.Element context msg) -> Ui.Element context msg
rowWithConstraints constraints attrs children =
    Ui.withChildren (Ui.Layout.rowWithConstraints constraints) attrs children
