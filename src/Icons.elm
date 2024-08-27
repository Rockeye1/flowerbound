module Icons exposing
    ( flip, download, upload, minus, plus, remove, reset
    , ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable
    , buck, butterfly, custom, doll, fiend, flower, vixen
    , toElement
    , icon
    )

{-|


## Actions

@docs flip, download, upload, minus, plus, remove, reset


## Moves

@docs ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable


## Gendertropes

@docs buck, butterfly, custom, doll, fiend, flower, vixen


## Utils

@docs toElement

-}

import Element exposing (Element, el)
import Phosphor
import Svg
import Svg.Attributes


icon : Phosphor.Icon -> Phosphor.IconWeight -> Element msg
icon input variant =
    input variant
        |> toElement


toElement : Phosphor.IconVariant -> Element msg
toElement variant =
    variant
        |> Phosphor.toHtml []
        |> Element.html
        |> el []


plus : Phosphor.IconVariant
plus =
    Phosphor.plus Phosphor.Bold


minus : Phosphor.IconVariant
minus =
    Phosphor.minus Phosphor.Bold


flip : Phosphor.IconVariant
flip =
    Phosphor.arrowsClockwise Phosphor.Bold


squish : Phosphor.IconVariant
squish =
    Phosphor.handPalm Phosphor.Duotone


grip : Phosphor.IconVariant
grip =
    Phosphor.handGrabbing Phosphor.Duotone


penetrate : Phosphor.IconVariant
penetrate =
    Phosphor.handTap Phosphor.Duotone


ensheathe : Phosphor.IconVariant
ensheathe =
    [ Svg.path
        [ Svg.Attributes.d "M25.22 19c0-4.18.04-8.54.04-12.72A2.3 2.3 0 0 0 22.98 4c-1.73 0-2.47 1.45-2.39 3.38l-1.02-3.65a2.4 2.4 0 0 0-2.8-1.57 2.3 2.3 0 0 0-1.63 2.8L15.9 8l-2.84-4.55c-.63-1.1-2.19-1.46-3.42-.75-1.09.63-1.47 2.3-.81 3.4l3.72 6.1c-2.82.94-4.9 2.92-5.71 5.24-.53 1.48.75 3.26 2.3 3.26 1.93 0 2.27-1.84 3.44-2.84a3.84 3.84 0 0 1 6.34 2.65 3.9 3.9 0 0 1-3.78 4.1 4.3 4.3 0 0 1-3.76-2.5c-.33-.89-1.4-1.4-2.35-1.4-1.53 0-2.67 1.57-2.22 3.13 1.22 3.65 4.71 6.01 8.78 6 5-.03 9.63-4.48 9.63-10.84"
        , Svg.Attributes.opacity "0.2"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d "M25.22 19c0-4.18.04-8.54.04-12.72A2.3 2.3 0 0 0 22.98 4c-1.73 0-2.47 1.45-2.39 3.38l-1.02-3.65a2.4 2.4 0 0 0-2.8-1.57 2.3 2.3 0 0 0-1.63 2.8L15.9 8l-2.84-4.55c-.63-1.1-2.19-1.46-3.42-.75-1.09.63-1.47 2.3-.81 3.4l3.72 6.1c-2.82.94-4.9 2.92-5.71 5.24-.53 1.48.75 3.26 2.3 3.26 1.93 0 2.27-1.84 3.44-2.84a3.84 3.84 0 0 1 6.34 2.65 3.9 3.9 0 0 1-3.78 4.1 4.3 4.3 0 0 1-3.76-2.5c-.33-.89-1.4-1.4-2.35-1.4-1.53 0-2.67 1.57-2.22 3.13 1.22 3.65 4.71 6.01 8.78 6 5-.03 9.63-4.48 9.63-10.84 M12.55 12.2s3.14-1.15 6.18.27"
        , Svg.Attributes.fill "none"
        ]
        []
    ]
        |> Svg.g
            [ Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.transform "scale(8)"
            ]
        |> List.singleton
        |> Phosphor.customIcon


squishable : Phosphor.IconVariant
squishable =
    Phosphor.cloud Phosphor.Duotone


grippable : Phosphor.IconVariant
grippable =
    Phosphor.joystick Phosphor.Duotone


penetrable : Phosphor.IconVariant
penetrable =
    Phosphor.keyhole Phosphor.Duotone


ensheatheable : Phosphor.IconVariant
ensheatheable =
    Phosphor.carrot Phosphor.Duotone


butterfly : Phosphor.IconVariant
butterfly =
    Phosphor.butterfly Phosphor.Duotone


flower : Phosphor.IconVariant
flower =
    Phosphor.flower Phosphor.Duotone


vixen : Phosphor.IconVariant
vixen =
    Phosphor.gitlabLogo Phosphor.Duotone


buck : Phosphor.IconVariant
buck =
    Phosphor.horse Phosphor.Duotone


fiend : Phosphor.IconVariant
fiend =
    Phosphor.pentagram Phosphor.Duotone


doll : Phosphor.IconVariant
doll =
    Phosphor.legoSmiley Phosphor.Duotone


custom : Phosphor.IconVariant
custom =
    Phosphor.pencil Phosphor.Duotone


upload : Phosphor.IconVariant
upload =
    Phosphor.upload Phosphor.Duotone


download : Phosphor.IconVariant
download =
    Phosphor.download Phosphor.Duotone


reset : Phosphor.IconVariant
reset =
    Phosphor.arrowCounterClockwise Phosphor.Regular


remove : Phosphor.IconVariant
remove =
    Phosphor.trash Phosphor.Duotone
