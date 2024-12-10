module Ui.WithContext.Shadow exposing (shadows)

import Ui.Shadow
import Ui.WithContext as Ui exposing (Attribute, Color)


shadows :
    List
        { x : Float
        , y : Float
        , size : Float
        , blur : Float
        , color : Color
        }
    -> Attribute context msg
shadows config =
    Ui.liftAttribute (Ui.Shadow.shadows config)
