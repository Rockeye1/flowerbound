module Route.Persona.Name_.Data__ exposing (ActionData, Data, Model, Msg, RouteParams, maybeCompress, maybeDecompress, personaFromSlug, route, toCard)

import Array
import BackendTask exposing (BackendTask)
import Base64
import Bit exposing (Bit)
import Bits
import Bits.Decode
import Bytes exposing (Bytes)
import Drawing
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage(..))
import FatalError exposing (FatalError)
import Flate
import Head
import Head.Seo as Seo
import Image
import Maybe.Extra
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona exposing (Persona)
import Rope
import Route exposing (Route)
import RouteBuilder exposing (App, StatefulRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Site
import Theme
import Url
import UrlPath exposing (UrlPath)
import View exposing (View)


type alias Model =
    Persona


type Msg
    = Flip
    | Update Persona


type alias RouteParams =
    { name : String
    , data : Maybe String
    }


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = action
        }
        |> RouteBuilder.buildWithSharedState
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action _ _ =
    BackendTask.succeed (Response.errorPage (InternalError "Go away"))


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect Msg )
init app _ =
    ( app.data
    , Effect.none
    )


personaFromSlug : String -> String -> Persona
personaFromSlug name slug =
    Maybe.Extra.andThen2
        (\fixedName bytes ->
            case Bits.Decode.run Persona.codec.decoder bytes of
                Ok partial ->
                    Just (Persona.fromPartial fixedName partial)

                Err _ ->
                    Nothing
        )
        (Url.percentDecode name)
        (slug
            |> String.replace "_" "/"
            |> String.replace "-" "+"
            |> Base64.toBytes
            |> Maybe.andThen
                (\bytes ->
                    bytes
                        |> Bits.fromBytes
                        |> maybeDecompress
                )
        )
        |> Maybe.withDefault Persona.default


maybeDecompress : List Bit -> Maybe (List Bit)
maybeDecompress input =
    case input of
        [] ->
            Just input

        Bit.O :: tail ->
            Just (List.drop 7 tail)

        Bit.I :: tail ->
            List.drop 7 tail
                |> Bits.toBytes
                |> Flate.inflate
                |> Maybe.map Bits.fromBytes


personaToSlug : Persona -> String
personaToSlug persona =
    persona
        |> Persona.toPartial
        |> Persona.codec.encoder
        |> Rope.toList
        |> Bits.toBytes
        |> maybeCompress
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "/" "_"
        |> String.replace "+" "-"


maybeCompress : Bytes -> Bytes
maybeCompress input =
    let
        compressed : Bytes
        compressed =
            Flate.deflate input

        bits : List Bit
        bits =
            if Bytes.width compressed < Bytes.width input then
                Bit.I :: List.repeat 7 Bit.O ++ Bits.fromBytes compressed

            else
                Bit.O :: List.repeat 7 Bit.O ++ Bits.fromBytes input
    in
    Bits.toBytes bits


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg, Maybe Shared.Msg )
update _ _ msg model =
    case msg of
        Update persona ->
            let
                newRoute : Route
                newRoute =
                    Route.Persona__Name___Data__
                        { name = Url.percentEncode persona.name
                        , data = Just (personaToSlug persona)
                        }
            in
            ( persona
            , newRoute |> Effect.SetRoute
            , Nothing
            )

        Flip ->
            ( model, Effect.none, Just Shared.Flip )


type alias Data =
    Persona


type alias ActionData =
    {}


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data params _ =
    params.data
        |> Maybe.withDefault ""
        |> personaFromSlug params.name
        |> Response.render
        |> BackendTask.succeed


head :
    App Persona ActionData RouteParams
    -> List Head.Tag
head app =
    let
        persona : Persona
        persona =
            app.data
    in
    Seo.summaryLarge
        { canonicalUrlOverride = Nothing
        , siteName = Site.manifest.name
        , image = cardImage persona
        , description = toDescription persona
        , locale = Nothing
        , title = title persona
        }
        |> Seo.website


cardImage : Persona -> Seo.Image
cardImage persona =
    { url =
        Pages.Url.fromPath
            [ "card"
            , "persona"
            , persona.name
            , personaToSlug persona
            ]
    , alt = "Card for " ++ persona.name
    , dimensions = Just cardImageSize
    , mimeType = Nothing
    }


title : Persona -> String
title persona =
    persona.name ++ " - " ++ Site.manifest.name


toDescription : Persona -> String
toDescription persona =
    [ persona.name
    , (Persona.gendertropeToRecord persona.gendertrope).name
    , "FIT " ++ String.fromInt persona.fitness
    , "GRC " ++ String.fromInt persona.grace
    , "ARD " ++ String.fromInt persona.ardor
    , "SAN " ++ String.fromInt persona.sanity
    , "PRW " ++ String.fromInt persona.prowess
    , "MOX " ++ String.fromInt persona.moxie
    ]
        |> String.join " "


cardImageSize :
    { width : number
    , height : number
    }
cardImageSize =
    let
        height : number
        height =
            (5 + 1) * 8 + 1
    in
    { width = height * 2
    , height = height
    }


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    { title = title app.data
    , body =
        Theme.el [ Theme.padding ] <|
            Persona.view
                { update = PagesMsg.fromMsg << Update
                , flip = PagesMsg.fromMsg Flip
                }
                { persona = model
                , flipped = shared.flipped
                }
    }


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none



-- CARD --


toCard : Persona -> BackendTask FatalError (Response.Response Never Never)
toCard persona =
    Drawing.getFont
        |> BackendTask.map
            (\font ->
                let
                    image : Drawing.Image
                    image =
                        (Theme.purpleHex * 256 + 0xFF)
                            |> Array.repeat cardImageSize.width
                            |> Array.repeat cardImageSize.height

                    padNumber : Int -> Int -> String
                    padNumber width value =
                        String.padLeft width ' ' (String.fromInt value)

                    description : String
                    description =
                        [ "FIT\u{2009}" ++ padNumber 2 persona.fitness
                        , "GRC\u{2009}" ++ padNumber 2 persona.grace
                        , "ARD\u{2009}" ++ padNumber 2 persona.ardor
                        , "SAN\u{2009}" ++ padNumber 2 persona.sanity
                        , "PRW\u{2009}" ++ padNumber 2 persona.prowess
                        , "MOX\u{2009}" ++ padNumber 2 persona.moxie
                        ]
                            |> String.join "\n"

                    meter : String -> Int -> String
                    meter label bonusToCap =
                        "Max\u{2009}"
                            ++ String.padRight 11 ' ' label
                            ++ "\u{2009}"
                            ++ padNumber 2 (20 + 2 * bonusToCap)

                    meters : String
                    meters =
                        [ meter "Stamina" 0
                        , meter "Satiation" persona.ardor
                        , meter "Craving" persona.sanity
                        , meter "Arousal" persona.prowess
                        , meter "Sensitivity" persona.moxie
                        , "Level\u{2009}bonus\u{2009} " ++ padNumber 5 (Persona.levelBonus persona)
                        ]
                            |> String.join "\n"
                in
                image
                    |> Drawing.drawImage 1 1 Drawing.flower
                    |> Drawing.drawImage (cardImageSize.width - 6) 1 Drawing.flower
                    |> Drawing.drawTextCenter font 1 persona.name
                    |> Drawing.drawTextCenter font (font.height + 2) (Persona.gendertropeToRecord persona.gendertrope).name
                    |> Drawing.drawText font 1 (font.height * 2 + 3) description
                    |> Drawing.drawText font 30 (font.height * 2 + 3) meters
                    |> Drawing.scaleBy (800 // cardImageSize.width)
                    |> Image.fromArray2d
                    |> Image.toPng
                    |> Response.bytesBody
                    |> Response.withHeader "Content-Type" "image/png"
            )
