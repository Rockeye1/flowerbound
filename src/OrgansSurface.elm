module OrgansSurface exposing (OrganKey, OrganPosition, height, organHeight, organWidth, view, width)

import Color
import Color.Oklch as Oklch exposing (Oklch)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Lazy
import Json.Decode
import List.Extra
import Persona.Data
import Phosphor exposing (IconVariant)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg
import Svg.Attributes
import Svg.Events
import Types exposing (Action(..), Appendage, Organ, Persona)
import Ui exposing (Color)


type alias OrganKey =
    ( Int, String )


type alias OrganPosition =
    ( Point2d Pixels (), Int )


organColors : List Color
organColors =
    [ Oklch.oklch 0.9 0.04 0
    , Oklch.oklch 0.9 0.04 (1 / 3)
    , Oklch.oklch 0.9 0.04 (2 / 3)
    ]
        |> List.map Oklch.toColor


width : number
width =
    1400


organWidth : number
organWidth =
    280


organHeight : number
organHeight =
    24 * 6 + 16


view :
    { mouseUp : msg
    , mouseDown : Point2d Pixels () -> msg
    , mouseMove : Point2d Pixels () -> msg
    }
    ->
        { a
            | organsPositions : Dict OrganKey OrganPosition
            , player : { p | persona : Persona }
            , others : List { p | persona : Persona }
            , dragging : Maybe b
        }
    -> Html msg
view config model =
    model.organsPositions
        |> Dict.toList
        |> List.sortBy (\( _, ( _, zOrder ) ) -> zOrder)
        |> List.concatMap (outerViewOrgan model)
        |> (::) (Svg.style [] [ Svg.text """svg text { cursor: default; }""" ])
        |> Svg.svg
            [ Svg.Attributes.width "100%"
            , [ 0
              , 0
              , width
              , height model
              ]
                |> List.map String.fromFloat
                |> String.join " "
                |> Svg.Attributes.viewBox
            , Svg.Events.custom "mousedown" (positionDecoder config.mouseDown)
            , case model.dragging of
                Just _ ->
                    Svg.Events.custom "mousemove" (positionDecoder config.mouseMove)

                Nothing ->
                    Svg.Attributes.class ""
            , Svg.Events.onMouseUp config.mouseUp
            ]


height :
    { a
        | organsPositions : Dict OrganKey OrganPosition
    }
    -> Float
height model =
    44 * toFloat (Dict.size model.organsPositions)


outerViewOrgan :
    { a
        | organsPositions : Dict ( Int, String ) ( Point2d Pixels (), comparable )
        , player : { p | persona : Persona }
        , others : List { p | persona : Persona }
        , dragging : Maybe b
    }
    -> (( ( Int, String ), ( Point2d Pixels (), comparable ) ) -> List (Html.Html c))
outerViewOrgan model ( ( i, organName ), ( pos, _ ) ) =
    let
        maybePersona : Maybe { p | persona : Persona }
        maybePersona =
            if i < 0 then
                Just model.player

            else
                List.Extra.getAt i model.others
    in
    case maybePersona of
        Nothing ->
            []

        Just { persona } ->
            case
                persona.gendertrope
                    |> Persona.Data.gendertropeToRecord
                    |> .organs
                    |> List.Extra.findMap
                        (\organ ->
                            if organ.name == organName then
                                Just ( organ, Nothing )

                            else
                                List.Extra.findMap
                                    (\appendage ->
                                        if organ.name ++ "-" ++ appendage.name == organName then
                                            -- TODO: this breaks lazy
                                            Just ( organ, Just appendage )

                                        else
                                            Nothing
                                    )
                                    organ.appendages
                        )
            of
                Nothing ->
                    []

                Just ( organ, appendage ) ->
                    let
                        color : Color
                        color =
                            organColors
                                |> List.drop (modBy (List.length organColors) i)
                                |> List.head
                                |> Maybe.withDefault Color.white
                    in
                    [ Html.Lazy.lazy5 viewOrgan persona color pos organ appendage ]


type TextAnchor
    = AnchorStart
    | AnchorEnd


viewOrgan : Persona -> Color -> Point2d Pixels () -> Organ -> Maybe Appendage -> Svg.Svg msg
viewOrgan persona color pos organ appendage =
    let
        { x, y } =
            Point2d.toPixels pos

        textAt :
            List (Svg.Attribute msg)
            ->
                { x : Float
                , y : Float
                , label : String
                , anchor : TextAnchor
                }
            -> Svg.Svg msg
        textAt attrs config =
            Svg.text_
                (Svg.Attributes.x
                    (String.fromFloat
                        (case config.anchor of
                            AnchorStart ->
                                8 + config.x

                            AnchorEnd ->
                                organWidth - 8 - config.x
                        )
                    )
                    :: Svg.Attributes.y (String.fromFloat (8 + 24 * config.y + 4))
                    :: Svg.Attributes.textAnchor
                        (case config.anchor of
                            AnchorStart ->
                                "start"

                            AnchorEnd ->
                                "end"
                        )
                    :: Svg.Attributes.dominantBaseline "hanging"
                    :: attrs
                )
                [ Svg.text config.label ]

        iconAt : Float -> Float -> IconVariant -> Html msg
        iconAt dx dy icon =
            icon
                |> Phosphor.withSize 24
                |> Phosphor.withSizeUnit "px"
                |> Phosphor.toHtml
                    [ Svg.Attributes.transform
                        ("translate("
                            ++ String.fromFloat dx
                            ++ " "
                            ++ String.fromFloat (6 + 24 * dy)
                            ++ ")"
                        )
                    ]

        iifLeft : (Organ -> Bool) -> (Appendage -> Bool) -> Action -> Float -> Svg.Svg msg
        iifLeft toConditionO toConditionA action dy =
            let
                condition : Bool
                condition =
                    appendage
                        |> Maybe.map toConditionA
                        |> Maybe.withDefault (toConditionO organ)
            in
            Svg.g
                [ if condition then
                    Svg.Attributes.fill "black"

                  else
                    Svg.Attributes.opacity "0.2"
                ]
                [ textAt []
                    { x = organWidth - 72 + 12
                    , y = dy
                    , label =
                        (if condition then
                            "⇒ "

                         else
                            ""
                        )
                            ++ "I"
                            ++ Types.actionToInitial action
                    , anchor = AnchorEnd
                    }
                , iconAt (72 - 16) dy (Types.actionToIsIcon action)
                , Svg.title [] [ Svg.text (Types.actionToIs action) ]
                ]

        iifRight : (Organ -> Bool) -> (Appendage -> Bool) -> Action -> Float -> Svg.Svg msg
        iifRight toConditionO toConditionA attribute dy =
            let
                condition : Bool
                condition =
                    appendage
                        |> Maybe.map toConditionA
                        |> Maybe.withDefault (toConditionO organ)
            in
            Svg.g
                [ if condition then
                    Svg.Attributes.fill "black"

                  else
                    Svg.Attributes.opacity "0.2"
                ]
                [ iconAt (organWidth - 72 - 16) dy (Types.actionToCanIcon attribute)
                , textAt []
                    { x = organWidth - 72 + 8
                    , y = dy
                    , label =
                        "C"
                            ++ Types.actionToInitial attribute
                            ++ (if condition then
                                    " ⇒"

                                else
                                    "  "
                               )
                    , anchor = AnchorStart
                    }
                , Svg.title [] [ Svg.text (Types.actionToCan attribute) ]
                ]
    in
    Svg.g
        [ Svg.Attributes.transform
            ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")")
        ]
        [ Svg.rect
            [ Svg.Attributes.width (String.fromFloat organWidth)
            , Svg.Attributes.height (String.fromFloat organHeight)
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill
                (Color.toCssString
                    (if appendage == Nothing then
                        color

                     else
                        let
                            oklch : Oklch
                            oklch =
                                color
                                    |> Oklch.fromColor
                        in
                        { oklch | chroma = 0.5 * oklch.chroma }
                            |> Oklch.toColor
                    )
                )
            ]
            []
        , Persona.Data.organTypeToIcon organ.type_
            |> Phosphor.withSize 24
            |> Phosphor.withSizeUnit "px"
            |> Phosphor.toHtml
                [ Svg.Attributes.transform
                    ("translate(" ++ String.fromFloat 8 ++ " " ++ String.fromFloat 4 ++ ")")
                ]
        , textAt []
            { x = 28
            , y = 0
            , label = Maybe.withDefault organ.name (Maybe.map .name appendage)
            , anchor = AnchorStart
            }
        , Persona.Data.gendertropeIcon persona.gendertrope
            |> Phosphor.withSize 24
            |> Phosphor.withSizeUnit "px"
            |> Phosphor.toHtml
                [ Svg.Attributes.transform
                    ("translate(" ++ String.fromFloat (organWidth - 8 - 24) ++ " " ++ String.fromFloat 4 ++ ")")
                ]
        , textAt []
            { x = 0
            , y = 1
            , label = "Contour: " ++ String.fromInt organ.contour
            , anchor = AnchorStart
            }
        , textAt []
            { x = 0
            , y = 1
            , label = "Erogeny: " ++ String.fromInt organ.erogeny
            , anchor = AnchorEnd
            }
        , iifRight .canSquish .canSquish Squishes 2
        , iifRight .canGrip .canGrip Grips 3
        , iifRight .canPenetrate .canPenetrate Penetrates 4
        , iifRight .canEnsheathe .canEnsheathe Ensheathes 5
        , iifLeft .isSquishable .isSquishable Squishes 2
        , iifLeft .isGrippable .isGrippable Grips 3
        , iifLeft .isPenetrable .isPenetrable Penetrates 4
        , iifLeft .isEnsheatheable .isEnsheatheable Ensheathes 5
        ]


positionDecoder :
    (Point2d Pixels () -> msg)
    ->
        Json.Decode.Decoder
            { message : msg
            , stopPropagation : Bool
            , preventDefault : Bool
            }
positionDecoder toMsg =
    Json.Decode.field "__svgCoordinates"
        (Json.Decode.map2
            (\x y ->
                { message = toMsg (Point2d.pixels x y)
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (Json.Decode.field "x" Json.Decode.float)
            (Json.Decode.field "y" Json.Decode.float)
        )
