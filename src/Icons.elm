module Icons exposing
    ( flip, download, upload, minus, plus, remove, reset
    , beginEncounter, rest, roll
    , ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable
    , buck, butterfly, custom, doll, fiend, flower, vixen
    , toElement
    )

{-|


## Actions

@docs flip, download, upload, minus, plus, remove, reset


## Encounter Actions

@docs beginEncounter, rest, roll


## Moves

@docs ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable


## Gendertropes

@docs buck, butterfly, custom, doll, fiend, flower, vixen


## Utils

@docs toElement

-}

import Phosphor exposing (IconVariant)
import Svg
import Svg.Attributes
import Ui exposing (Element, el)


toElement : IconVariant -> Element msg
toElement variant =
    variant
        |> Phosphor.toHtml []
        |> Ui.html
        |> el []


plus : IconVariant
plus =
    Phosphor.plus Phosphor.Bold


minus : IconVariant
minus =
    Phosphor.minus Phosphor.Bold


flip : IconVariant
flip =
    Phosphor.arrowsClockwise Phosphor.Bold


squish : IconVariant
squish =
    Phosphor.handPalm Phosphor.Duotone


grip : IconVariant
grip =
    Phosphor.handGrabbing Phosphor.Duotone


penetrate : IconVariant
penetrate =
    Phosphor.handTap Phosphor.Duotone


ensheathe : IconVariant
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


squishable : IconVariant
squishable =
    Phosphor.cloud Phosphor.Duotone


grippable : IconVariant
grippable =
    Phosphor.joystick Phosphor.Duotone


penetrable : IconVariant
penetrable =
    Phosphor.keyhole Phosphor.Duotone


ensheatheable : IconVariant
ensheatheable =
    Phosphor.carrot Phosphor.Duotone


butterfly : IconVariant
butterfly =
    Phosphor.butterfly Phosphor.Duotone


flower : IconVariant
flower =
    Phosphor.flower Phosphor.Duotone


vixen : IconVariant
vixen =
    Phosphor.gitlabLogo Phosphor.Duotone


buck : IconVariant
buck =
    Phosphor.horse Phosphor.Duotone


fiend : IconVariant
fiend =
    Phosphor.pentagram Phosphor.Duotone


doll : IconVariant
doll =
    Phosphor.legoSmiley Phosphor.Duotone


custom : IconVariant
custom =
    Phosphor.pencil Phosphor.Duotone


upload : IconVariant
upload =
    Phosphor.upload Phosphor.Duotone


download : IconVariant
download =
    Phosphor.download Phosphor.Duotone


reset : IconVariant
reset =
    Phosphor.arrowCounterClockwise Phosphor.Regular


remove : IconVariant
remove =
    Phosphor.trash Phosphor.Duotone


rest : IconVariant
rest =
    Phosphor.bed Phosphor.Duotone


roll : IconVariant
roll =
    Phosphor.diceSix Phosphor.Duotone


beginEncounter : IconVariant
beginEncounter =
    Phosphor.intersect Phosphor.Duotone
