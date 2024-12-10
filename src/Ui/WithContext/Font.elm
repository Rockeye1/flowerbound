module Ui.WithContext.Font exposing (Font, alignLeft, alignRight, bold, center, color, family, italic, monospace, size, strike, underline)

import Html.Attributes
import Ui.Font
import Ui.WithContext as Ui exposing (Attribute, Color)


type alias Font =
    Ui.Font.Font


strike : Attribute context msg
strike =
    Ui.liftAttribute Ui.Font.strike


italic : Attribute context msg
italic =
    Ui.liftAttribute Ui.Font.italic


underline : Attribute context msg
underline =
    Ui.liftAttribute Ui.Font.underline


bold : Attribute context msg
bold =
    Ui.liftAttribute Ui.Font.bold


center : Attribute context msg
center =
    Ui.liftAttribute Ui.Font.center


color : Color -> Attribute context msg
color value =
    Ui.htmlAttribute (Html.Attributes.style "color" (Ui.colorToCss value))


alignLeft : Attribute context msg
alignLeft =
    Ui.liftAttribute Ui.Font.alignLeft


alignRight : Attribute context msg
alignRight =
    Ui.liftAttribute Ui.Font.alignRight


size : Int -> Attribute context msg
size value =
    Ui.liftAttribute (Ui.Font.size value)


family : List Font -> Attribute context msg
family value =
    Ui.liftAttribute (Ui.Font.family value)


monospace : Font
monospace =
    Ui.Font.monospace
