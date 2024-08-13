module Route.Persona.Slug_ exposing (ActionData, Data, Model, Msg, encodeNonnegativeInt, encodePositiveInt, parsePositiveInt, route)

import BackendTask exposing (BackendTask)
import Base64
import Bit exposing (Bit(..))
import BitParser
import Bits
import Bytes exposing (Bytes)
import Bytes.Encode
import Codec.Bare as Codec
import Effect exposing (Effect)
import Element exposing (el)
import ErrorPage exposing (ErrorPage(..))
import FatalError exposing (FatalError)
import Flate
import Head
import Head.Seo as Seo
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Route
import RouteBuilder exposing (App, StatefulRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Theme
import Types exposing (Persona)
import UrlPath exposing (UrlPath)
import View exposing (View)
import View.Persona


type alias Model =
    { flipped : Bool
    , persona : Persona
    }


type Msg
    = Flip
    | Update Persona


type alias RouteParams =
    { slug : String }


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
      }
    , Effect.none
    )


personaFromSlug : String -> Persona
personaFromSlug slug =
    slug
        |> String.replace "_" "/"
        |> String.replace "-" "+"
        |> Base64.toBytes
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen decodePersona
        |> Maybe.withDefault defaultPersona


personaToSlug : Persona -> String
personaToSlug persona =
    persona
        |> encodePersona
        |> Flate.deflate
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "/" "_"
        |> String.replace "+" "-"


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ _ msg model =
    case msg of
        Update persona ->
            ( { model | persona = persona }
            , Route.Persona__Slug_
                { slug = personaToSlug persona
                }
                |> Effect.SetRoute
            )

        Flip ->
            ( { model | flipped = not model.flipped }, Effect.none )


type alias Data =
    Persona


type alias ActionData =
    {}


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data { slug } _ =
    BackendTask.succeed (Response.render (personaFromSlug slug))


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
    }


decodePersona : Bytes -> Maybe Persona
decodePersona bytes =
    BitParser.run personaParser bytes


personaParser : BitParser.Parser Persona
personaParser =
    BitParser.succeed Persona
        |> BitParser.andMap
            (parseNonnegativeInt |> BitParser.andThen BitParser.stringDecoder)
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
    [ encodeNonnegativeInt (Bytes.Encode.getStringWidth persona.name)
    , persona.name
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Bits.fromBytes
    , encodeNonnegativeInt (persona.fitness - 2)
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
    ]
        |> List.concat
        |> Bits.toIntUnsigned8s
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


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
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Flowerbound"
        , image =
            { url = Pages.Url.external <| "/persona/image/" ++ app.routeParams.slug
            , alt = "Card for " ++ app.data.name
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "FIT " ++ String.fromInt app.data.fitness
        , locale = Nothing
        , title = "Persona: " ++ app.data.name
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view _ _ model =
    { title = "Placeholder - Blog.Slug_"
    , body =
        el [ Theme.padding ] <|
            View.Persona.view
                { update = PagesMsg.fromMsg << Update
                , flip = PagesMsg.fromMsg Flip
                }
                model
    }


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none
