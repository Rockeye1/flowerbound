module Ui.WithContext.Shadow exposing (shadows)

import Color
import Color.Oklch as Oklch
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
shadows configs =
    Ui.liftAttribute (Ui.Shadow.shadows (List.map convertShadow configs))


convertShadow :
    { x : Float
    , y : Float
    , size : Float
    , blur : Float
    , color : Ui.Color
    }
    ->
        { x : Float
        , y : Float
        , size : Float
        , blur : Float
        , color : Color.Color
        }
convertShadow { x, y, size, blur, color } =
    { x = x
    , y = y
    , size = size
    , blur = blur
    , color = Oklch.toColor color
    }
