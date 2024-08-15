module Route.Persona.Name_.Data__ exposing (ActionData, Data, Model, Msg, cardImageSize, defaultPersona, encodeNonnegativeInt, encodePositiveInt, parseNonnegativeInt, parsePositiveInt, personaFromSlug, route, title, toCard, toDescription)

import Array
import BackendTask exposing (BackendTask)
import Base64
import Bit exposing (Bit(..))
import BitParser
import Bits
import Bytes exposing (Bytes)
import Drawing
import Effect exposing (Effect)
import Element
import ErrorPage exposing (ErrorPage(..))
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Http
import Image
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Route exposing (Route)
import RouteBuilder exposing (App, StatefulRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Site
import Theme
import Types exposing (Gendertrope(..), Persona)
import Url
import UrlPath exposing (UrlPath)
import View exposing (View)
import View.Persona


type alias Model =
    { flipped : Bool
    , persona : Persona
    , image : Maybe String
    }


type Msg
    = Flip
    | Update Persona
    | GotImage (Result Http.Error String)


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
        |> RouteBuilder.buildWithLocalState
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
    ( { flipped = False
      , persona = app.data
      , image = Nothing
      }
    , Effect.none
    )


personaFromSlug : String -> String -> Persona
personaFromSlug name slug =
    slug
        |> String.replace "_" "/"
        |> String.replace "-" "+"
        |> Base64.toBytes
        |> Maybe.andThen
            (\inflated ->
                Maybe.andThen (\n -> decodePersona n inflated) (Url.percentDecode name)
            )
        |> Maybe.withDefault defaultPersona


personaToSlug : Persona -> String
personaToSlug persona =
    persona
        |> encodePersona
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "/" "_"
        |> String.replace "+" "-"


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
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
            ( { model
                | persona = persona
                , image = Nothing
              }
            , Effect.batch
                [ newRoute
                    |> Effect.SetRoute
                , Http.get
                    { url = (cardImage persona).url |> Pages.Url.toString
                    , expect = Http.expectString GotImage
                    }
                    |> Effect.fromCmd
                ]
            )

        Flip ->
            ( { model | flipped = not model.flipped }, Effect.none )

        GotImage (Err _) ->
            ( model, Effect.none )

        GotImage (Ok bytes) ->
            ( bytes
                |> Base64.toBytes
                |> Maybe.andThen Image.decode
                |> Maybe.map Image.toPngUrl
                |> Maybe.map (\image -> { model | image = Just image })
                |> Maybe.withDefault model
            , Effect.none
            )


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


defaultPersona : Persona
defaultPersona =
    { name = "Cinderella Sheen"
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , stamina = 0
    , satiation = 0
    , craving = 0
    , arousal = 0
    , sensitivity = 0

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0

    --
    , gendertrope = TheButterfly
    }


decodePersona : String -> Bytes -> Maybe Persona
decodePersona name bytes =
    BitParser.run (personaParser name) (Bits.fromBytes bytes)


personaParser : String -> BitParser.Parser Persona
personaParser name =
    BitParser.succeed (Persona name)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) parseNonnegativeInt)
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap parseNonnegativeInt
        |> BitParser.andMap
            parseGendertrope


parseGendertrope : BitParser.Parser Gendertrope
parseGendertrope =
    parseNonnegativeInt
        |> BitParser.andThen
            (\i ->
                case i of
                    0 ->
                        BitParser.succeed TheButterfly

                    _ ->
                        BitParser.fail
            )


encodeGendertrope : Gendertrope -> List Bit
encodeGendertrope gendertrope =
    case gendertrope of
        TheButterfly ->
            encodeNonnegativeInt 0


parseNonnegativeInt : BitParser.Parser Int
parseNonnegativeInt =
    BitParser.map (\n -> n - 1) parsePositiveInt


parsePositiveInt : BitParser.Parser Int
parsePositiveInt =
    BitParser.loop
        (\n ->
            BitParser.bit
                |> BitParser.andThen
                    (\bit ->
                        if bit == O then
                            BitParser.succeed (BitParser.Done n)

                        else
                            BitParser.bits n
                                |> BitParser.map (\bits -> BitParser.Loop (Bits.toIntUnsigned (I :: bits)))
                    )
        )
        1


encodePersona : Persona -> Bytes
encodePersona persona =
    [ encodeNonnegativeInt (persona.fitness - 2)
    , encodeNonnegativeInt (persona.grace - 2)
    , encodeNonnegativeInt (persona.ardor - 2)
    , encodeNonnegativeInt (persona.sanity - 2)
    , encodeNonnegativeInt (persona.prowess - 2)
    , encodeNonnegativeInt (persona.moxie - 2)
    , encodeNonnegativeInt persona.stamina
    , encodeNonnegativeInt persona.satiation
    , encodeNonnegativeInt persona.craving
    , encodeNonnegativeInt persona.arousal
    , encodeNonnegativeInt persona.sensitivity
    , encodeNonnegativeInt persona.euphoriaPoints
    , encodeNonnegativeInt persona.ichorPoints
    , encodeNonnegativeInt persona.numinousPoints
    , encodeGendertrope persona.gendertrope
    ]
        |> List.concat
        |> BitParser.bitsToBytes


encodeNonnegativeInt : Int -> List Bit
encodeNonnegativeInt n =
    encodePositiveInt (n + 1)


encodePositiveInt : Int -> List Bit
encodePositiveInt i =
    if i < 1 then
        []

    else
        encodePositiveIntHelper i [ O ]


encodePositiveIntHelper : Int -> List Bit -> List Bit
encodePositiveIntHelper n acc =
    if n == 1 then
        acc

    else
        let
            length : Int
            length =
                ceiling (logBase 2 (toFloat n + 1))
        in
        encodePositiveIntHelper (length - 1) (Bits.fromIntUnsigned length n ++ acc)


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
    "FIT "
        ++ String.fromInt persona.fitness
        ++ " GRC "
        ++ String.fromInt persona.grace
        ++ " ARD "
        ++ String.fromInt persona.ardor
        ++ " SAN "
        ++ String.fromInt persona.sanity
        ++ " PRW "
        ++ String.fromInt persona.prowess
        ++ " MOX "
        ++ String.fromInt persona.moxie


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
view app _ model =
    { title = title app.data
    , body =
        Theme.column [ Theme.padding ]
            [ View.Persona.view
                { update = PagesMsg.fromMsg << Update
                , flip = PagesMsg.fromMsg Flip
                }
                model
            , case model.image of
                Just src ->
                    Element.image []
                        { src = src
                        , description = "Preview"
                        }

                Nothing ->
                    Element.none
            ]
    }


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none



-- CARD --


toCard : Persona -> BackendTask FatalError (Response.Response Never Never)
toCard persona =
    Drawing.getFont
        |> BackendTask.andThen
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
                        [ "FIT " ++ padNumber 2 persona.fitness
                        , "GRC " ++ padNumber 2 persona.grace
                        , "ARD " ++ padNumber 2 persona.ardor
                        , "SAN " ++ padNumber 2 persona.sanity
                        , "PRW " ++ padNumber 2 persona.prowess
                        , "MOX " ++ padNumber 2 persona.moxie
                        ]
                            |> String.join "\n"

                    meter : String -> Int -> Int -> String
                    meter label value bonusToCap =
                        String.padRight 12 ' ' label
                            ++ padNumber 2 value
                            ++ "/"
                            ++ padNumber 2 (20 + 2 * bonusToCap)

                    meters : String
                    meters =
                        [ meter "Stamina" persona.stamina 0
                        , meter "Satiation" persona.satiation persona.ardor
                        , meter "Craving" persona.craving persona.sanity
                        , meter "Arousal" persona.arousal persona.prowess
                        , meter "Sensitivity" persona.sensitivity persona.moxie
                        ]
                            |> String.join "\n"
                in
                image
                    |> Drawing.drawImage 1 1 Drawing.flower
                    |> Drawing.drawImage (cardImageSize.width - 6) 1 Drawing.flower
                    |> Drawing.drawTextCenter font 1 persona.name
                    |> Drawing.drawTextCenter font (font.height + 2) (gendertropeToString persona.gendertrope)
                    |> Drawing.drawText font 1 (font.height * 2 + 3) description
                    |> Drawing.drawText font ((font.width + 1) * 7 + 1) (font.height * 2 + 3) meters
                    |> Drawing.scaleBy (800 // cardImageSize.width)
                    |> Image.fromArray2d
                    |> Image.toPng
                    |> Response.bytesBody
                    |> Response.withHeader "Content-Type" "image/png"
                    |> BackendTask.succeed
            )


gendertropeToString : Gendertrope -> String
gendertropeToString gendertrope =
    case gendertrope of
        TheButterfly ->
            "The Butterfly"
