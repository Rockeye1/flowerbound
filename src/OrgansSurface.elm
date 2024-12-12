module OrgansSurface exposing (OrganKey, OrganPosition, height, organHeight, organWidth, view, width)

import Dict exposing (Dict)
import Html exposing (Html)
import Icons
import Json.Decode
import List.Extra
import List.NonEmpty exposing (NonEmpty)
import Persona
import Persona.Data
import Phosphor exposing (IconVariant)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Set
import Svg
import Svg.Attributes
import Svg.Events
import Theme
import Types exposing (Action(..), Appendage, Organ, Persona)
import Ui.WithContext as Ui exposing (Color)


type alias OrganKey =
    ( Int, String )


type alias OrganPosition =
    { position : Point2d Pixels ()
    , zIndex : Int
    , show : Bool
    }


organColors : NonEmpty Color
organColors =
    ( 0, [ 1 / 3, 2 / 3 ] )
        |> List.NonEmpty.map Persona.organColorFromReducedHue


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
    , showAppendages : Int -> String -> msg
    , hideOrganOrAppendage : Int -> String -> msg
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
        |> List.sortBy (\( _, { zIndex } ) -> zIndex)
        |> List.filterMap
            (\(( ( i, organName ), _ ) as arg) ->
                outerViewOrgan
                    { showAppendages = config.showAppendages i organName
                    , hideOrganOrAppendage = config.hideOrganOrAppendage i organName
                    }
                    model
                    arg
            )
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
    -> number
height model =
    Dict.foldl
        (\( i, _ ) { show } ( acc, seen ) ->
            if Set.member i seen then
                if show then
                    ( acc + 32, seen )

                else
                    ( acc, seen )

            else
                ( acc + organHeight + 8, Set.insert i seen )
        )
        ( 8, Set.empty )
        model.organsPositions
        |> Tuple.first


outerViewOrgan :
    { showAppendages : msg
    , hideOrganOrAppendage : msg
    }
    ->
        { a
            | organsPositions : Dict OrganKey OrganPosition
            , player : { p | persona : Persona }
            , others : List { p | persona : Persona }
            , dragging : Maybe b
        }
    -> ( OrganKey, OrganPosition )
    -> Maybe (Html.Html msg)
outerViewOrgan config model ( ( i, organName ), { position, show } ) =
    if not show then
        Nothing

    else
        let
            maybePersona : Maybe { p | persona : Persona }
            maybePersona =
                if i < 0 then
                    Just model.player

                else
                    List.Extra.getAt i model.others
        in
        maybePersona
            |> Maybe.andThen
                (\{ persona } ->
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
                        |> Maybe.map
                            (\( organ, appendage ) ->
                                let
                                    color : Color
                                    color =
                                        (Persona.toColors persona).organ
                                            |> Maybe.withDefault (cyclicGetAt i organColors)
                                in
                                viewOrgan config persona color position organ appendage
                            )
                )


cyclicGetAt : Int -> NonEmpty a -> a
cyclicGetAt index list =
    let
        go : Int -> NonEmpty a -> a
        go i ( head, tail ) =
            if i <= 0 then
                head

            else
                go (i - 1) (Maybe.withDefault list (List.NonEmpty.fromList tail))
    in
    go (modBy (List.NonEmpty.length list) index) list


type TextAnchor
    = AnchorStart
    | AnchorEnd


viewOrgan :
    { showAppendages : msg
    , hideOrganOrAppendage : msg
    }
    -> Persona
    -> Color
    -> Point2d Pixels ()
    -> Organ
    -> Maybe Appendage
    -> Svg.Svg msg
viewOrgan config persona color pos organ appendage =
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
        textAt attrs textConfig =
            Svg.text_
                (Svg.Attributes.x
                    (String.fromFloat
                        (case textConfig.anchor of
                            AnchorStart ->
                                8 + textConfig.x

                            AnchorEnd ->
                                organWidth - 8 - textConfig.x
                        )
                    )
                    :: Svg.Attributes.y (String.fromFloat (8 + 24 * textConfig.y + 4))
                    :: Svg.Attributes.textAnchor
                        (case textConfig.anchor of
                            AnchorStart ->
                                "start"

                            AnchorEnd ->
                                "end"
                        )
                    :: Svg.Attributes.dominantBaseline "hanging"
                    :: attrs
                )
                [ Svg.text textConfig.label ]

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

        centerButtons : List (Svg.Svg msg)
        centerButtons =
            if appendage == Nothing && not (List.isEmpty organ.appendages) then
                [ centerButton 0 "Show appendage" Icons.plus config.showAppendages
                , centerButton 1 "Hide" Icons.hide config.hideOrganOrAppendage
                ]

            else
                [ centerButton 0 "Hide" Icons.hide config.hideOrganOrAppendage ]

        centerButton : Int -> String -> IconVariant -> msg -> Svg.Svg msg
        centerButton index label icon msg =
            let
                buttonY : Float
                buttonY =
                    organHeight - 40 * (toFloat index + 1)
            in
            Svg.g
                [ Svg.Events.onClick msg
                , Svg.Attributes.cursor "pointer"
                ]
                [ Svg.rect
                    [ Svg.Attributes.x (String.fromFloat (organWidth / 2 - 16))
                    , Svg.Attributes.y (String.fromFloat buttonY)
                    , Svg.Attributes.width "32"
                    , Svg.Attributes.height "32"
                    , Svg.Attributes.fill (Ui.colorToCss (Theme.lighten (Theme.toAccent color)))
                    , Svg.Attributes.stroke "black"
                    ]
                    []
                , icon
                    |> Phosphor.withSize 24
                    |> Phosphor.withSizeUnit "px"
                    |> Phosphor.toHtml
                        [ Svg.Attributes.transform
                            ("translate("
                                ++ String.fromFloat (organWidth / 2 - 12)
                                ++ " "
                                ++ String.fromFloat (buttonY + 4)
                                ++ ")"
                            )
                        ]
                , Svg.title [] [ Svg.text label ]
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
                (Ui.colorToCss
                    (if appendage == Nothing then
                        color

                     else
                        Theme.desaturate color
                    )
                )
            ]
            []
        , let
            ( variant, flipped ) =
                Persona.Data.organTypeToIcon organ.type_ appendage
          in
          variant
            |> Phosphor.withSize 24
            |> Phosphor.withSizeUnit "px"
            |> Phosphor.toHtml
                [ Svg.Attributes.transform
                    (if flipped then
                        "translate(32 4) scale(-1 1)"

                     else
                        "translate(8 4)"
                    )
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
                    ("translate(" ++ String.fromFloat (organWidth - 8 - 24) ++ " 4)")
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
        , Svg.g [] centerButtons
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
