module Icons exposing
    ( flip, download, upload, minus, plus, remove, reset, delete
    , beginEncounter, rest, roll
    , ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable
    , buck, butterfly, custom, doll, fiend, flower, vixen
    , breasts, hands, hips, legs, mouth, other, phallic, prehensile, yonic
    , toElement
    )

{-|


## Actions

@docs flip, download, upload, minus, plus, remove, reset, delete


## Encounter Actions

@docs beginEncounter, rest, roll


## Moves

@docs ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable


## Gendertropes

@docs buck, butterfly, custom, doll, fiend, flower, vixen


## Organ types

@docs breasts, hands, hips, legs, mouth, other, phallic, prehensile, yonic


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


mouth : IconVariant
mouth =
    [ Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M13.1,36.8c4.1,1.4,5.1,1.6,11.6,0.1"
        ]
        []
    , Svg.path
        [ Svg.Attributes.opacity "0.2"
        , Svg.Attributes.d "M58.9,36.8c-3.7-2.4-8.1-7.6-12.8-11.7c-1.1-1-2.4-2.1-4.8-1l-3.1,1.7c-0.7,0.4-1,0.6-2.2,0.6s-1.4-0.1-2.2-0.6l-3.1-1.7c-2.4-1.1-3.7,0-4.8,1c-4.7,4.1-9.1,9.2-12.8,11.7l12.2,11.7c2.1,2,4.8,3.7,10.6,3.7s8.6-1.7,10.6-3.7L58.9,36.8z M36,40.4c-6.3,0-8-4-11.3-3.5c5.5-4.2,8.5-0.5,11.3-0.5s5.8-3.7,11.3,0.5C44,36.4,42.3,40.4,36,40.4z"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M58.9,36.8c-4.1,1.4-5.1,1.6-11.6,0.1"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M13.1,36.8c4.1,1.4,5.1,1.6,11.6,0.1"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M13.1,36.8l12.2,11.7c2.1,2,4.8,3.7,10.6,3.7s8.6-1.7,10.6-3.7l12.2-11.7"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M13.1,36.8c3.7-2.4,8.1-7.6,12.8-11.7c1.1-1,2.4-2.1,4.8-1l3.1,1.7c0.7,0.4,1,0.6,2.2,0.6s1.4-0.1,2.2-0.6l3.1-1.7c2.4-1.1,3.7,0,4.8,1c4.7,4.1,9.1,9.2,12.8,11.7"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M58.9,36.8c-4.1,1.4-5.1,1.6-11.6,0.1"
        ]
        []
    , Svg.path
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.d "M36,40.4c-6.3,0-8-4-11.3-3.5c5.5-4.2,8.5-0.5,11.3-0.5s5.8-3.7,11.3,0.5C44,36.4,42.3,40.4,36,40.4z"
        ]
        []
    ]
        |> Svg.g
            [ Svg.Attributes.transform "matrix(5,0,0,5,-52,-61.626993)"
            , Svg.Attributes.strokeWidth "3.2"
            ]
        |> List.singleton
        |> Phosphor.customIcon


hands : IconVariant
hands =
    Phosphor.hand Phosphor.Duotone


breasts : IconVariant
breasts =
    [ Svg.circle
        [ Svg.Attributes.cx "128"
        , Svg.Attributes.cy "128"
        , Svg.Attributes.r "96"
        , Svg.Attributes.fill "none"
        ]
        []
    , Svg.circle
        [ Svg.Attributes.cx "128"
        , Svg.Attributes.cy "160"
        , Svg.Attributes.r "32"
        , Svg.Attributes.opacity "0.2"
        ]
        []
    , Svg.circle
        [ Svg.Attributes.cx "128"
        , Svg.Attributes.cy "160"
        , Svg.Attributes.r "12"
        ]
        []
    ]
        |> Svg.g
            [ Svg.Attributes.strokeWidth "16"
            ]
        |> List.singleton
        |> Phosphor.customIcon


hips : IconVariant
hips =
    Phosphor.cloud Phosphor.Duotone


yonic : IconVariant
yonic =
    Phosphor.genderFemale Phosphor.Duotone


phallic : IconVariant
phallic =
    Phosphor.genderMale Phosphor.Duotone


legs : IconVariant
legs =
    [ Svg.path
        [ Svg.Attributes.d "M23.4375 11.8125C23.4375 11.8125 14.7656 4.26563 13.3594 3.01563C11.9531 1.76562 8.14374 1.21249 6.59374 4.18749C5.04374 7.16249 6.83869 9.47196 8.03124 10.1875C11.1953 12.0859 17.9187 15.3875 18.4687 15.8125C19.3628 16.5034 17.4477 17.1106 17.1016 19.9453C16.7657 22.6963 17.6797 24.0625 17.6797 26.4766C17.6797 27.1276 17.2266 28.1094 17.2266 28.5625C17.2266 29.5067 17.8221 29.9844 18.9844 29.9844H24.9609C25.5156 29.9844 25.9375 29.6484 25.9375 29C25.9375 28.3516 25.3646 28.0443 25.0312 28.0234C24.4104 28.0234 23.4649 27.6819 22.625 27.2266C21.6914 26.7205 20.9033 26.0547 21.1562 25.1562C21.7604 23.0104 23.1625 18.1937 23.9375 16.0937C24.7812 13.7187 24.0833 12.4271 23.4375 11.8125Z"
        , Svg.Attributes.opacity "0.2"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d "M23.4375 11.8125C23.4375 11.8125 14.7656 4.26563 13.3594 3.01563C11.9531 1.76562 8.14374 1.21249 6.59374 4.18749C5.04374 7.16249 6.83869 9.47196 8.03124 10.1875C11.1953 12.0859 17.9187 15.3875 18.4687 15.8125C19.3628 16.5034 17.4477 17.1106 17.1016 19.9453C16.7657 22.6963 17.6797 24.0625 17.6797 26.4766C17.6797 27.1276 17.2266 28.1094 17.2266 28.5625C17.2266 29.5067 17.8221 29.9844 18.9844 29.9844H24.9609C25.5156 29.9844 25.9375 29.6484 25.9375 29C25.9375 28.3516 25.3646 28.0443 25.0312 28.0234C24.4104 28.0234 23.4649 27.6819 22.625 27.2266C21.6914 26.7205 20.9033 26.0547 21.1562 25.1562C21.7604 23.0104 23.1625 18.1937 23.9375 16.0937C24.7812 13.7187 24.0833 12.4271 23.4375 11.8125Z"
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


prehensile : IconVariant
prehensile =
    Phosphor.handGrabbing Phosphor.Duotone


other : IconVariant
other =
    Phosphor.question Phosphor.Duotone


delete : Phosphor.IconVariant
delete =
    Phosphor.trash Phosphor.Duotone
