module OrgansSurface exposing (OrganKey, OrganPosition, height, organHeight, organWidth, view, width)

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
import Types exposing (Action(..), Organ, Persona)


type alias OrganKey =
    ( Int, String )


type alias OrganPosition =
    ( Point2d Pixels (), Int )


organColors : List String
organColors =
    [ "#f0e0e0"
    , "#e0f0e0"
    , "#e0e0f0"
    ]


width : number
width =
    1600


height : Float
height =
    width * 9 / 16


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
            , persona : Persona
            , others : List Persona
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
              , height
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


outerViewOrgan :
    { a
        | organsPositions : Dict ( Int, String ) ( Point2d Pixels (), comparable )
        , persona : Persona
        , others : List Persona
        , dragging : Maybe b
    }
    -> (( ( Int, String ), ( Point2d Pixels (), comparable ) ) -> List (Html.Html c))
outerViewOrgan model ( ( i, organName ), ( pos, _ ) ) =
    let
        maybePersona : Maybe Persona
        maybePersona =
            if i < 0 then
                Just model.persona

            else
                List.Extra.getAt i model.others
    in
    case maybePersona of
        Nothing ->
            []

        Just persona ->
            case
                persona.gendertrope
                    |> Persona.Data.gendertropeToRecord
                    |> .organs
                    |> List.Extra.find (\organ -> organ.name == organName)
            of
                Nothing ->
                    []

                Just organ ->
                    let
                        color : String
                        color =
                            organColors
                                |> List.drop (modBy (List.length organColors) i)
                                |> List.head
                                |> Maybe.withDefault "white"
                    in
                    [ Html.Lazy.lazy4 viewOrgan persona color pos organ ]


type TextAnchor
    = AnchorStart
    | AnchorEnd


viewOrgan : Persona -> String -> Point2d Pixels () -> Organ -> Svg.Svg msg
viewOrgan persona color pos organ =
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

        iifLeft : Bool -> Action -> Float -> Svg.Svg msg
        iifLeft condition action dy =
            Svg.g
                [ if condition then
                    Svg.Attributes.fill "black"

                  else
                    Svg.Attributes.fill "gray"
                , if condition then
                    Svg.Attributes.color "black"

                  else
                    Svg.Attributes.color "gray"
                ]
                [ textAt []
                    { x = 0
                    , y = dy
                    , label = "⇒ I" ++ Types.actionToInitial action
                    , anchor = AnchorStart
                    }
                , iconAt (72 - 16) dy (Types.actionToIsIcon action)
                , Svg.title [] [ Svg.text (Types.actionToIs action) ]
                ]

        iifRight : Bool -> Action -> Float -> Svg.Svg msg
        iifRight condition attribute dy =
            Svg.g
                [ if condition then
                    Svg.Attributes.fill "black"

                  else
                    Svg.Attributes.fill "gray"
                , if condition then
                    Svg.Attributes.color "black"

                  else
                    Svg.Attributes.color "gray"
                ]
                [ iconAt (organWidth - 72 - 16) dy (Types.actionToCanIcon attribute)
                , textAt []
                    { x = 0
                    , y = dy
                    , label = "C" ++ Types.actionToInitial attribute ++ " ⇒"
                    , anchor = AnchorEnd
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
            , Svg.Attributes.fill color
            ]
            []
        , Persona.Data.gendertropeIcon persona.gendertrope
            |> Phosphor.withSize 24
            |> Phosphor.withSizeUnit "px"
            |> Phosphor.toHtml
                [ Svg.Attributes.transform
                    ("translate(" ++ String.fromFloat 8 ++ " " ++ String.fromFloat 8 ++ ")")
                ]
        , textAt []
            { x = 32
            , y = 0
            , label = organ.name
            , anchor = AnchorStart
            }
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
        , iifRight organ.canSquish Squishes 2
        , iifRight organ.canGrip Grips 3
        , iifRight organ.canPenetrate Penetrates 4
        , iifRight organ.canEnsheathe Ensheathes 5
        , iifLeft organ.isSquishable Squishes 2
        , iifLeft organ.isGrippable Grips 3
        , iifLeft organ.isPenetrable Penetrates 4
        , iifLeft organ.isEnsheatheable Ensheathes 5
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
