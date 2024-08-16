module Icons exposing (ensheathe, ensheatheable, flip, grip, grippable, minus, penetrable, penetrate, plus, squish, squishable)

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


squish : Element msg
squish =
    icon Phosphor.handPalm Phosphor.Duotone


grip : Element msg
grip =
    icon Phosphor.handGrabbing Phosphor.Duotone


penetrate : Element msg
penetrate =
    icon Phosphor.handTap Phosphor.Duotone


ensheathe : Element msg
ensheathe =
    icon Phosphor.handFist Phosphor.Duotone


squishable : Element msg
squishable =
    icon Phosphor.heart Phosphor.Duotone


grippable : Element msg
grippable =
    icon Phosphor.joystick Phosphor.Duotone


penetrable : Element msg
penetrable =
    icon Phosphor.keyhole Phosphor.Duotone


ensheatheable : Element msg
ensheatheable =
    icon Phosphor.carrot Phosphor.Duotone


icon : Phosphor.Icon -> Phosphor.IconWeight -> Element msg
icon input variant =
    input variant
        |> Phosphor.toHtml []
        |> Element.html
        |> el []
