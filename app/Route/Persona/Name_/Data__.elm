module Route.Persona.Name_.Data__ exposing (ActionData, Data, Model, Msg, RouteParams, maybeCompress, maybeDecompress, partialPersonaFromSlug, route, toCard)

import Array
import BackendTask exposing (BackendTask)
import Base64
import Bit exposing (Bit)
import Bits
import Bits.Decode
import Bytes exposing (Bytes)
import Drawing
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Flate
import Head
import Head.Seo as Seo
import Image
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Types exposing (Gendertrope, GendertropeRecord, PartialPersona, Persona)
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
    BackendTask.succeed (Response.errorPage ErrorPage.InvalidRequest)


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect Msg )
init app _ =
    let
        ( name, partialPersona ) =
            app.data

        maybeGendertrope : Maybe GendertropeRecord
        maybeGendertrope =
            app.url
                |> Maybe.andThen .fragment
                |> Maybe.andThen slugToBytes
                |> Maybe.andThen
                    (\bytes ->
                        Bits.Decode.run Persona.gendertropeRecordCodec.decoder bytes
                            |> Result.toMaybe
                    )
    in
    ( Persona.fromPartial name partialPersona maybeGendertrope
    , Effect.none
    )


partialPersonaFromSlug : String -> Maybe PartialPersona
partialPersonaFromSlug slug =
    slugToBytes slug
        |> Maybe.andThen
            (\slugBytes ->
                case Bits.Decode.run Persona.codec.decoder slugBytes of
                    Ok partial ->
                        Just partial

                    Err _ ->
                        Nothing
            )


slugToBytes : String -> Maybe (List Bit)
slugToBytes slug =
    slug
        |> String.replace "_" "/"
        |> String.replace "-" "+"
        |> Base64.toBytes
        |> Maybe.andThen
            (\bytes ->
                bytes
                    |> Bits.fromBytes
                    |> maybeDecompress
            )


maybeDecompress : List Bit -> Maybe (List Bit)
maybeDecompress input =
    case input of
        [] ->
            Just input

        Bit.O :: tail ->
            Just tail

        Bit.I :: tail ->
            tail
                |> List.drop 7
                |> Bits.toBytes
                |> Flate.inflate
                |> Maybe.map Bits.fromBytes


partialPersonaToSlug : PartialPersona -> String
partialPersonaToSlug partialPersona =
    partialPersona
        |> Persona.codec.encoder
        |> Rope.toList
        |> Bits.toBytes
        |> bytesToSlug


bytesToSlug : Bytes -> String
bytesToSlug bytes =
    bytes
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
                Bit.O :: Bits.fromBytes input
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
                        , data = Just (partialPersonaToSlug (Persona.toPartial persona))
                        }
            in
            ( persona
            , Effect.SetRoute newRoute (gendertropeToHash persona.gendertrope)
            , Nothing
            )

        Flip ->
            ( model, Effect.none, Just Shared.Flip )


gendertropeToHash : Gendertrope -> String
gendertropeToHash gendertrope =
    case gendertrope of
        Persona.Types.Custom record ->
            Persona.gendertropeRecordCodec.encoder record
                |> Rope.toList
                |> Bits.toBytes
                |> bytesToSlug

        _ ->
            ""


type alias Data =
    ( String, Maybe PartialPersona )


type alias ActionData =
    {}


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data params _ =
    ( Url.percentDecode params.name
        |> Maybe.withDefault params.name
    , Maybe.andThen partialPersonaFromSlug params.data
    )
        |> Response.render
        |> BackendTask.succeed


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    case app.data of
        ( _, Nothing ) ->
            []

        ( name, Just partialPersona ) ->
            Seo.summaryLarge
                { canonicalUrlOverride = Nothing
                , siteName = Site.manifest.name
                , image = cardImage ( name, partialPersona )
                , description = toDescription ( name, partialPersona )
                , locale = Nothing
                , title = title name
                }
                |> Seo.website


cardImage : ( String, PartialPersona ) -> Seo.Image
cardImage ( name, persona ) =
    { url =
        Pages.Url.fromPath
            [ "card"
            , "persona"
            , Url.percentEncode name
            , partialPersonaToSlug persona
            ]
    , alt = "Card for " ++ name
    , dimensions = Just cardImageSize
    , mimeType = Nothing
    }


title : String -> String
title name =
    name ++ " - " ++ Site.manifest.name


toDescription : ( String, PartialPersona ) -> String
toDescription ( name, partialPersona ) =
    [ name
    , Persona.partialGendertropeName partialPersona.gendertrope
    , "FIT " ++ String.fromInt partialPersona.fitness
    , "GRC " ++ String.fromInt partialPersona.grace
    , "ARD " ++ String.fromInt partialPersona.ardor
    , "SAN " ++ String.fromInt partialPersona.sanity
    , "PRW " ++ String.fromInt partialPersona.prowess
    , "MOX " ++ String.fromInt partialPersona.moxie
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
    { title = title (Tuple.first app.data)
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


toCard : String -> PartialPersona -> BackendTask FatalError (Response.Response Never Never)
toCard name partialPersona =
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
                        [ "FIT\u{2009}" ++ padNumber 2 partialPersona.fitness
                        , "GRC\u{2009}" ++ padNumber 2 partialPersona.grace
                        , "ARD\u{2009}" ++ padNumber 2 partialPersona.ardor
                        , "SAN\u{2009}" ++ padNumber 2 partialPersona.sanity
                        , "PRW\u{2009}" ++ padNumber 2 partialPersona.prowess
                        , "MOX\u{2009}" ++ padNumber 2 partialPersona.moxie
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
                        , meter "Satiation" partialPersona.ardor
                        , meter "Craving" partialPersona.sanity
                        , meter "Arousal" partialPersona.prowess
                        , meter "Sensitivity" partialPersona.moxie
                        , "Level\u{2009}bonus\u{2009} " ++ padNumber 5 (Persona.levelBonus partialPersona)
                        ]
                            |> String.join "\n"
                in
                image
                    |> Drawing.drawImage 1 1 Drawing.flower
                    |> Drawing.drawImage (cardImageSize.width - 6) 1 Drawing.flower
                    |> Drawing.drawTextCenter font 1 name
                    |> Drawing.drawTextCenter font (font.height + 2) (Persona.partialGendertropeName partialPersona.gendertrope)
                    |> Drawing.drawText font 1 (font.height * 2 + 3) description
                    |> Drawing.drawText font 30 (font.height * 2 + 3) meters
                    |> Drawing.scaleBy (800 // cardImageSize.width)
                    |> Image.fromArray2d
                    |> Image.toPng
                    |> Response.bytesBody
                    |> Response.withHeader "Content-Type" "image/png"
            )
