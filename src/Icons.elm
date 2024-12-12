module Icons exposing
    ( flip, download, upload, minus, plus, remove, reset, delete, hide, show
    , beginEncounter, rest, roll
    , ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable
    , buck, butterfly, custom, doll, fiend, flower, vixen
    , breasts, hands, hips, legs, mouth, other, phallic, prehensile, yonic
    , toElement, toElementFlippable
    )

{-|


## Actions

@docs flip, download, upload, minus, plus, remove, reset, delete, hide, show


## Encounter Actions

@docs beginEncounter, rest, roll


## Moves

@docs ensheathe, ensheatheable, grip, grippable, penetrable, penetrate, squish, squishable


## Gendertropes

@docs buck, butterfly, custom, doll, fiend, flower, vixen


## Organ types

@docs breasts, hands, hips, legs, mouth, other, phallic, prehensile, yonic


## Utils

@docs toElement, toElementFlippable

-}

import Html.Attributes
import Phosphor exposing (IconVariant)
import Svg
import Svg.Attributes
import Ui.WithContext as Ui exposing (Element, el)


toElement : IconVariant -> Element context msg
toElement variant =
    variant
        |> Phosphor.toHtml []
        |> Ui.html
        |> el []


toElementFlippable : ( IconVariant, Bool ) -> Element context msg
toElementFlippable ( variant, flipped ) =
    variant
        |> Phosphor.toHtml []
        |> Ui.html
        |> el
            (if flipped then
                [ Ui.htmlAttribute (Html.Attributes.style "transform" "scale(-1, 1)") ]

             else
                []
            )


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
        , Svg.Attributes.r "48"
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
        |> Svg.g [ Svg.Attributes.strokeWidth "16" ]
        |> List.singleton
        |> Phosphor.customIcon


hips : IconVariant
hips =
    [ Svg.path
        [ Svg.Attributes.d "M75.22 223.96c-2.27-15.35-3.32-20.6-15.97-37.5-15.1-20.16-31.27-49.5-23.16-80.36a147 147 0 0 1 5.04-15.66c13.72-35.47 34.16-42.7 33.37-58.4h107c-.8 15.7 19.65 22.93 33.37 58.4a147 147 0 0 1 5.04 15.66c8.1 30.87-8.07 60.2-23.16 80.36-12.65 16.9-13.7 22.15-15.97 37.5z"
        , Svg.Attributes.opacity ".2"
        ]
        []
    , Svg.g
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.strokeWidth "26.667"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.transform "matrix(.6 0 0 .6 -322 -321.91)"
        ]
        [ Svg.path
            [ Svg.Attributes.id "hips-a"
            , Svg.Attributes.d "M660.83 589.92c1.32 26.17-32.75 38.22-55.62 97.34a244 244 0 0 0-8.39 26.1c-13.52 51.44 13.44 100.33 38.6 133.93 21.07 28.15 22.83 36.91 26.61 62.5"
            ]
            []
        , Svg.path
            [ Svg.Attributes.id "hips-b"
            , Svg.Attributes.d "M656.2 824.96c36.6 7.75 67.65-4.75 82.94-31.15 13.98-24.14 14.8-59.9-5.34-102.44"
            ]
            []
        , Svg.use
            [ Svg.Attributes.xlinkHref "#hips-a"
            , Svg.Attributes.transform "matrix(-1 0 0 1 1500 0)"
            ]
            []
        , Svg.use
            [ Svg.Attributes.xlinkHref "#hips-b"
            , Svg.Attributes.transform "matrix(-1 0 0 1 1500 0)"
            ]
            []
        , Svg.path [ Svg.Attributes.d "M750 793.8V909.8" ] []
        ]
    ]
        |> Phosphor.customIcon


yonic : IconVariant
yonic =
    Phosphor.flowerLotus Phosphor.Duotone


phallic : IconVariant
phallic =
    [ Svg.g
        [ Svg.Attributes.transform "rotate(180,128,128)"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M94.77,213.23a36.77,36.77,0,0,1-52,0h0a36.77,36.77,0,0,1,0-52L172,32l60,60-24,8Z"
            , Svg.Attributes.opacity "0.2"
            ]
            []
        , Svg.path
            [ Svg.Attributes.d "M232,92 L208,100 L94.77,213.23 a36.77,36.77,0,0,1-52,0 h0 a36.77,36.77,0,0,1,0-52 L172,32 "
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "currentColor"
            , Svg.Attributes.strokeWidth "16"
            ]
            []
        ]
    ]
        |> Svg.g
            [ Svg.Attributes.strokeWidth "16"
            , Svg.Attributes.transform "matrix(1 0 0 1 0 0)"
            ]
        |> List.singleton
        |> Phosphor.customIcon


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
    [ Svg.path
        [ Svg.Attributes.d "m 320.031,482.751 c 82.612,-58.804 67.125,-198.108 -39.124,-245.28 -134.93,-59.91 -104.62,-158.366 -9.97,-176.376 53.675,-10.214 97.9,17.11 106.314,50.812 12.404,49.682 -39.41002,85.00702 -114.47,38.53 72.17601,105.70249 168.275,38.942 161.19,-44.374 -4.14,-48.648 -49.446,-92.985 -131,-92.468 l -10e-4,-0.001 c -2.632,0.016 -5.295,0.076 -8,0.187 -5.774,0.24 -12.015,1.07 -18.126,1.75 -169.278,18.935 -231.652,152.474 -88.75,258.72 87.533,65.08 84.216,128.05 -87.594,208.5"
        , Svg.Attributes.opacity "0.2"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d "m 320.031,482.751 c 82.612,-58.804 67.125,-198.108 -39.124,-245.28 -134.93,-59.91 -104.62,-158.366 -9.97,-176.376 53.675,-10.214 97.9,17.11 106.314,50.812 12.404,49.682 -39.41002,85.00702 -114.47,38.53 72.17601,105.70249 168.275,38.942 161.19,-44.374 -4.14,-48.648 -49.446,-92.985 -131,-92.468 l -10e-4,-0.001 c -2.632,0.016 -5.295,0.076 -8,0.187 -5.774,0.24 -12.015,1.07 -18.126,1.75 -169.278,18.935 -231.652,152.474 -88.75,258.72 87.533,65.08 84.216,128.05 -87.594,208.5"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.strokeWidth "35.556"
        ]
        []
    ]
        |> Svg.g
            [ Svg.Attributes.transform "matrix(0.45,0,0,0.45,24.318725,32.646196)"
            ]
        |> List.singleton
        |> Phosphor.customIcon


other : IconVariant
other =
    Phosphor.question Phosphor.Duotone


delete : Phosphor.IconVariant
delete =
    Phosphor.trash Phosphor.Duotone


show : Phosphor.IconVariant
show =
    Phosphor.eye Phosphor.Duotone


hide : Phosphor.IconVariant
hide =
    Phosphor.eyeSlash Phosphor.Duotone
