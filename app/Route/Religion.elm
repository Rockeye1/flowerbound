module Route.Religion exposing (ActionData, Data, Model, Msg, Religion, RouteParams, data, image, route)

import BackendTask exposing (BackendTask)
import Color exposing (Color)
import Effect exposing (Effect)
import Element exposing (paragraph)
import Element.Background as Background
import Element.Font as Font
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import LanguageTag.Language
import LanguageTag.Region
import List.Extra
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (StatelessRoute)
import Shared
import Site
import Svg exposing (Attribute)
import Theme
import TypedSvg exposing (circle, g, line, style, svg, text_)
import TypedSvg.Attributes exposing (class, dominantBaseline, fill, id, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, foreignObject, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..))
import UrlPath
import View exposing (View)


type alias Msg =
    ()


type alias Model =
    {}


type alias RouteParams =
    {}


type alias Data =
    { intro : List String
    , religions : List Religion
    }


type alias ActionData =
    Never


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


init : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect msg )
init _ _ =
    ( {}, Effect.none )


update : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect msg )
update _ _ _ model =
    ( model, Effect.none )


subscriptions : RouteParams -> UrlPath.UrlPath -> Shared.Model -> Model -> Sub msg
subscriptions _ _ _ _ =
    Sub.none


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head _ =
    let
        defaultHead :
            { canonicalUrlOverride : Maybe String
            , siteName : String
            , image : Seo.Image
            , description : String
            , locale : Maybe ( LanguageTag.Language.Language, LanguageTag.Region.Region )
            , title : String
            }
        defaultHead =
            Site.defaultSummary
    in
    Seo.summary
        { defaultHead
            | title = title
            , description = "Religions of the Starheart Lodge"
        }
        |> Seo.website


title : String
title =
    Site.manifest.name ++ " - Religions"


type Religion
    = Named
        { name : String
        , story : List String
        , deity :
            { name : String
            , title : String
            , description : String
            , holySymbol : String
            , realm :
                { name : String
                , meaning : String
                }
            }
        }
    | Apotheosis { description : List String }


data : BackendTask FatalError Data
data =
    BackendTask.succeed
        { intro =
            [ "While it might seem strange for an afterlife to have its own religion, the power of mythology is only enhanced when it is not corrupted into justifications and excuses for judgement, oppression, or bigotry. The power of mythology is only enhanced when it is not corrupted into a substitute for curiosity, investigation, and scientific inquiry."
            , "In the Starheart Lodge, the _Mythos of the Five-Pointed Star_ serves as both a shared conceptual language for expressing oneself, as well as a common ground for creativity and fetishistic inspiration. It is a religion that holds sacred the fundamental message that its gods welcome every willing soul as their equal."
            , "(Mouse over the names to get more details.)"
            ]
        , religions =
            [ dreams
            , invention
            , daring
            , perversity
            , apotheosis
            ]
        }


dreams : Religion
dreams =
    Named
        { name = "Dreams"
        , story =
            [ "That night, everyone in the tribe stepped into the beautiful forest, and the skies were kind, and the trees loved you and all gave fruit that was good to eat and didn't have seeds that stuck in your teeth, and the moss underfoot was always soft and friendly, and the night air was always warm enough but not so hot you would sweat, and the only watching eyes or hungry mouths found lurking in the leaves were those of a new lover waiting to pounce, and you were always safe, and they all lived happily ever after."
            ]
        , deity =
            { name = "Lilith"
            , title = "Innocent Seducer"
            , description = "soft hearts and natural joys. She is the wish for the world to be forgiving and kind. She is lust unrestrained by shame. She is for when 'knowing better' _isn't_ actually better."
            , holySymbol = "snake eating an apple"
            , realm =
                { name = "Salvus Weald"
                , meaning = "safe woods"
                }
            }
        }


invention : Religion
invention =
    Named
        { name = "Invention"
        , story =
            [ "They spoke to the wind in verse and images, and so raised great mountains into the sky and hung them on the rush of standing above everything on the highest pinnacle of the highest house with the wind in your face, and on those mountains their children and their children's children built beautiful houses made of the generosity of trees and the hardness of thought, and in them loved and lived and thought high thoughts, and they lived happily ever after."
            ]
        , deity =
            { name = "Venus"
            , title = "Thoughtful Romantic"
            , description = "enduring passion and ambitious planning. She is the wish for the world to yield to cleverness and effort. She is forethought and anticipation. She is for when perfection is less perfect than your imperfect ideal."
            , holySymbol = "stairway into clouds"
            , realm =
                { name = "Venereum Consilium"
                , meaning = "romantic plan"
                }
            }
        }


perversity : Religion
perversity =
    Named
        { name = "Perversity"
        , story =
            [ "They confessed to each other the darkness in their hearts, and the things they would do and the wishes they carried. The truths that would send their lovers running in terror or recoiling in disgust. The desires they nursed in secret, curling in terror from their own reflections. And by doing so they found in each other the beautiful reflections of that darkness. They found in each fear, another's wish. They found that in love, every nightmare can become a dream."
            , "United by the miracle of darkness welcomed and open hearts healed, they dove deeper and deeper into the depths of their truths, until they had come to their home, and there among the loving monstrosities, they played and played and lived happily ever after."
            ]
        , deity =
            { name = "Sade"
            , title = "Beautiful Monster"
            , description = "dangerous kinks. She is the wish for the world to restore, or never take in the first place, what may be destroyed in the pursuit of desire. She is the scene and the safeword. She is for when you struggle to imagine that the extreme is compatible with the good."
            , holySymbol = "churning waters"
            , realm =
                { name = "Mare Veritatis"
                , meaning = "sea of truth"
                }
            }
        }


daring : Religion
daring =
    Named
        { name = "Daring"
        , story =
            [ "They flew high, high into the air and hurled the fury of their hearts down into the center of the highest mountain where it lit the peak with the fires of exhilaration. Each dared the next, and they went to play among the snow and molten stone. They lived happily ever after."
            ]
        , deity =
            { name = "Valkyr"
            , title = "Valiant Challenger"
            , description = "bravery and competition. She is the wish for the world to be _un_forgiving, _un_yielding, and unaccommodating... but only when that's more fun than the alternative. She is the venture and the high-stakes gamble. She is for when you chafe at safety and wish to discover your true limits."
            , holySymbol = "erupting volcano"
            , realm =
                { name = "Virtus Apicem"
                , meaning = "peak of bravery"
                }
            }
        }


apotheosis : Religion
apotheosis =
    Apotheosis
        { description =
            [ "*You* are the fifth point of the star. Within every soul is the power to be the god(dess) of their own inner world, to define their own ideals and craft their own concepts."
            , "Your holy symbol, be it holy only to you, lays nestled in your heart, and your own divine realm is the web that connects all of your dreams and wishes. It is not a place, but rather a lens that you carry with you everywhere you go."
            ]
        }


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg Msg)
view app shared _ =
    { title = title
    , body =
        Theme.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color Theme.lightPurple
            , Theme.padding
            ]
            (Theme.pageTitle "World Guide: Religion"
                :: viewIntro app.data
                ++ [ Element.el
                        [ Element.width Element.fill
                        , Element.height <| Element.maximum (shared.height - 270) Element.fill
                        ]
                        (Element.html <| image app.data.religions)
                   ]
            )
    }


baseWidth : number
baseWidth =
    2000


religionRadius : Float
religionRadius =
    baseWidth / 4


viewIntro : Data -> List (Element.Element (PagesMsg Msg))
viewIntro { intro } =
    List.indexedMap
        (\j line ->
            line
                |> String.split "_"
                |> List.indexedMap
                    (\i segment ->
                        if modBy 2 i == 1 then
                            Element.el [ Font.italic ] (Element.text segment)

                        else
                            Element.text segment
                    )
                |> paragraph
                    (if j == 2 then
                        [ Font.size 16 ]

                     else
                        []
                    )
        )
        intro


image : List Religion -> Html msg
image religions =
    let
        ( viewBox, nodes ) =
            children religions
    in
    svg
        [ viewBox
        , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        , strokeWidth (baseWidth / 100)
        ]
        nodes


children : List Religion -> ( Attribute msg, List (Svg msg) )
children religions =
    let
        count : Int
        count =
            List.length religions

        angle : Float
        angle =
            2 * pi / toFloat count

        ( ( minX, maxX ), ( minY, maxY ) ) =
            let
                updateRange : ( Float, Float ) -> Float -> ( Float, Float )
                updateRange ( mn, mx ) v =
                    ( min mn v, max mx v )
            in
            List.foldl
                (\( x, y, _ ) ( w, h ) ->
                    ( updateRange w x
                    , updateRange h y
                    )
                )
                ( ( 0, 0 ), ( 0, 0 ) )
                religionsWithPositions

        religionsWithPositions : List ( Float, Float, Religion )
        religionsWithPositions =
            List.indexedMap
                (\i religion ->
                    ( baseWidth * sin (angle * toFloat (i + 1))
                    , baseWidth * cos (angle * toFloat (i + 1))
                    , religion
                    )
                )
                religions

        lines : List (Svg msg)
        lines =
            List.Extra.lift2
                (\( lx, ly, _ ) ( rx, ry, _ ) ->
                    if lx == rx && ly == ry || ((lx - rx) ^ 2 + (ly - ry) ^ 2 <= (religionRadius * 6) ^ 2) then
                        g [] []

                    else
                        line
                            [ x1 lx
                            , x2 rx
                            , y1 ly
                            , y2 ry
                            ]
                            []
                )
                religionsWithPositions
                religionsWithPositions

        padding : Float
        padding =
            religionRadius + baseWidth / 50

        maxScale : Float
        maxScale =
            2.9
    in
    ( viewBox
        (minX - padding)
        (minY - padding)
        (maxX - minX + 2 * padding)
        (maxY - minY + 2 * padding)
    , [ style []
            [ text
                """
                .fast-hidable {
                    opacity: 0.001;
                }

                .religion > .hidable {
                    opacity: 1;
                }

                .religion:hover > .hidable {
                    opacity: 0;
                }

                .religion > .showable {
                    opacity: 0;
                }

                .religion:hover > .showable {
                    opacity: 1;
                }

                .religion > * {
                    transition: transform 2s, opacity 0.5s, stroke-width 2s;
                }
                """
            , religionsWithPositions
                |> List.indexedMap
                    (\i ( x, y, _ ) ->
                        """#religion-""" ++ String.fromInt i ++ """ > * {
                            transform: matrix(1, 0, 0, 1, """ ++ String.fromFloat x ++ """, """ ++ String.fromFloat y ++ """);
                        }

                        #religion-""" ++ String.fromInt i ++ """ > .movable {
                            stroke-width: """ ++ String.fromFloat (baseWidth / 100) ++ """px;
                        }

                        #religion-""" ++ String.fromInt i ++ """:hover > .movable {
                            stroke-width: """ ++ String.fromFloat (baseWidth / (100 * maxScale)) ++ """px;
                            transform: matrix(""" ++ String.fromFloat maxScale ++ """, 0, 0, """ ++ String.fromFloat maxScale ++ """, 0, 0);
                        }"""
                    )
                |> String.join "\n\n"
                |> text
            ]
      , circle
            [ r baseWidth
            , fill PaintNone
            , stroke (Paint purple)
            ]
            []

      --   , g [ stroke (Paint purple) ] lines
      , g
            [ fontSize (baseWidth / 14) ]
            (List.indexedMap viewReligion religions)
      ]
    )


viewReligion : Int -> Religion -> Svg msg
viewReligion i religion =
    let
        name : String
        name =
            case religion of
                Named named ->
                    named.name

                Apotheosis _ ->
                    "Apotheosis"
    in
    g
        [ id ("religion-" ++ String.fromInt i)
        , class [ "religion" ]
        ]
        [ circle
            [ r (religionRadius * 1.2)
            , fill (Paint lightPurple)
            , stroke (Paint purple)
            , class [ "fast-hidable" ]
            ]
            []
        , circle
            [ r religionRadius
            , fill (Paint lightPurple)
            , stroke (Paint purple)
            , class [ "movable" ]
            ]
            []
        , text_
            [ textAnchor AnchorMiddle
            , dominantBaseline DominantBaselineCentral
            , class [ "movable", "hidable" ]
            ]
            [ text name
            ]
        , g
            [ class [ "movable", "showable" ]
            ]
            [ let
                pWidth : Float
                pWidth =
                    religionRadius * 1.6

                pHeight : Float
                pHeight =
                    religionRadius * 2
              in
              foreignObject
                [ x -(pWidth / 2)
                , y -(pHeight / 2)
                , width pWidth
                , height pHeight
                ]
                [ Html.p
                    [ Html.Attributes.attribute "xmlns" "http://www.w3.org/1999/xhtml"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "font-size" "28px"
                    , Html.Attributes.style "gap" "8px"
                    , Html.Attributes.style "margin" "0"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "height" "inherit"
                    ]
                    (List.map (Html.span [])
                        ([ Html.b [] [ Html.u [] [ Html.text ("The Apoasteri of " ++ name) ] ] ]
                            :: (case religion of
                                    Named named ->
                                        (named.story
                                            |> List.map (\line -> [ Html.i [] [ Html.text line ] ])
                                        )
                                            ++ [ [ Html.b [] [ Html.text named.deity.name ]
                                                 , Html.text ", the "
                                                 , Html.b [] [ Html.text named.deity.title ]
                                                 , Html.text " is the goddess of "
                                                 , named.deity.description
                                                    |> parsed
                                                 ]
                                               , [ Html.text "Her holy symbol is "
                                                 , Html.b [] [ Html.text named.deity.holySymbol ]
                                                 , Html.text ", and her divine realm is called "
                                                 , Html.b [] [ Html.text named.deity.realm.name ]
                                                 , Html.text (", the " ++ named.deity.realm.meaning ++ ". It is not a place, but rather a nature any place can take on.")
                                                 ]
                                               ]

                                    Apotheosis { description } ->
                                        description
                                            |> List.map (\line -> [ parsed line ])
                               )
                        )
                    )
                ]
            ]
        ]


parsed : String -> Html msg
parsed line =
    let
        splitB segment =
            segment
                |> String.split "*"
                |> List.indexedMap
                    (\i subSegment ->
                        if modBy 2 i == 1 then
                            Html.b [] [ Html.text subSegment ]

                        else
                            Html.text subSegment
                    )
    in
    line
        |> String.split "_"
        |> List.indexedMap
            (\i segment ->
                if modBy 2 i == 1 then
                    [ Html.i [] (splitB segment) ]

                else
                    splitB segment
            )
        |> List.concat
        |> Html.span []


purple : Color
purple =
    Color.rgb255 0x80 0 0x80


lightPurple : Color
lightPurple =
    Color.rgb255 0xE6 0xCC 0xE6
