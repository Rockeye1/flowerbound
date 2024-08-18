module Icons exposing (buck, butterfly, custom, doll, ensheathe, ensheatheable, fiend, flip, flower, grip, grippable, minus, penetrable, penetrate, plus, squish, squishable, vixen)

import Element exposing (Element, el)
import Phosphor
import Svg
import Svg.Attributes


icon : Phosphor.Icon -> Phosphor.IconWeight -> Element msg
icon input variant =
    input variant
        |> Phosphor.toHtml []
        |> Element.html
        |> el []


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
    [ Svg.path
        [ Svg.Attributes.d "M25.22 19c0-4.18.04-8.54.04-12.72A2.3 2.3 0 0 0 22.98 4c-1.73 0-2.47 1.45-2.39 3.38l-1.02-3.65a2.4 2.4 0 0 0-2.8-1.57 2.3 2.3 0 0 0-1.63 2.8L15.9 8l-2.84-4.55c-.63-1.1-2.19-1.46-3.42-.75-1.09.63-1.47 2.3-.81 3.4l3.72 6.1c-2.82.94-4.9 2.92-5.71 5.24-.53 1.48.75 3.26 2.3 3.26 1.93 0 2.27-1.84 3.44-2.84a3.84 3.84 0 0 1 6.34 2.65 3.9 3.9 0 0 1-3.78 4.1 4.3 4.3 0 0 1-3.76-2.5c-.33-.89-1.4-1.4-2.35-1.4-1.53 0-2.67 1.57-2.22 3.13 1.22 3.65 4.71 6.01 8.78 6 5-.03 9.63-4.48 9.63-10.84"
        , Svg.Attributes.opacity ".2"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d "M25.22 19c0-4.18.04-8.54.04-12.72A2.3 2.3 0 0 0 22.98 4c-1.73 0-2.47 1.45-2.39 3.38l-1.02-3.65a2.4 2.4 0 0 0-2.8-1.57 2.3 2.3 0 0 0-1.63 2.8L15.9 8l-2.84-4.55c-.63-1.1-2.19-1.46-3.42-.75-1.09.63-1.47 2.3-.81 3.4l3.72 6.1c-2.82.94-4.9 2.92-5.71 5.24-.53 1.48.75 3.26 2.3 3.26 1.93 0 2.27-1.84 3.44-2.84a3.84 3.84 0 0 1 6.34 2.65 3.9 3.9 0 0 1-3.78 4.1 4.3 4.3 0 0 1-3.76-2.5c-.33-.89-1.4-1.4-2.35-1.4-1.53 0-2.67 1.57-2.22 3.13 1.22 3.65 4.71 6.01 8.78 6 5-.03 9.63-4.48 9.63-10.84"
        , Svg.Attributes.fill "none"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d "M12.55 12.2s3.14-1.15 6.18.27"
        , Svg.Attributes.fill "none"
        ]
        []
    ]
        |> Svg.svg
            [ Svg.Attributes.width "1em"
            , Svg.Attributes.height "1em"
            , Svg.Attributes.fill "currentColor"
            , Svg.Attributes.stroke "currentColor"
            , Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            , Svg.Attributes.viewBox "0 0 32 32"
            ]
        |> Element.html
        |> el []


squishable : Element msg
squishable =
    icon Phosphor.cloud Phosphor.Duotone


grippable : Element msg
grippable =
    icon Phosphor.joystick Phosphor.Duotone


penetrable : Element msg
penetrable =
    icon Phosphor.keyhole Phosphor.Duotone


ensheatheable : Element msg
ensheatheable =
    icon Phosphor.carrot Phosphor.Duotone


butterfly : Element msg
butterfly =
    icon Phosphor.butterfly Phosphor.Duotone


flower : Element msg
flower =
    icon Phosphor.flower Phosphor.Duotone


vixen : Element msg
vixen =
    icon Phosphor.pawPrint Phosphor.Duotone


buck : Element msg
buck =
    icon Phosphor.horse Phosphor.Duotone


fiend : Element msg
fiend =
    icon Phosphor.pentagram Phosphor.Duotone


doll : Element msg
doll =
    icon Phosphor.legoSmiley Phosphor.Duotone


custom : Element msg
custom =
    icon Phosphor.pencil Phosphor.Duotone
