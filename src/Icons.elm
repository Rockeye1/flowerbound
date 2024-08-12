module Icons exposing (flip, minus, plus)

import Element exposing (Element, el)
import Phosphor


plus : Element msg
plus =
    icon Phosphor.plus Phosphor.Bold


minus : Element msg
minus =
    icon Phosphor.minus Phosphor.Bold


flip : Element msg
flip =
    icon Phosphor.arrowsClockwise Phosphor.Bold


icon : Phosphor.Icon -> Phosphor.IconWeight -> Element msg
icon input variant =
    input variant
        |> Phosphor.toHtml []
        |> Element.html
        |> el []
