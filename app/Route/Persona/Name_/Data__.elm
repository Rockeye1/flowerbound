module Route.Persona.Name_.Data__ exposing (ActionData, Data, Model, Msg, cardImageSize, personaFromSlug, route, title, toCard, toDescription)

import Array
import BackendTask exposing (BackendTask)
import Base64
import Bit exposing (Bit(..))
import BitParser
import Bits
import Drawing
import Effect exposing (Effect)
import Element
import ErrorPage exposing (ErrorPage(..))
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Http
import Image
import Maybe.Extra
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona exposing (Gendertrope(..), Persona)
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
    Maybe.Extra.andThen2 (\fixedName bytes -> BitParser.run (Persona.parser fixedName) (Bits.fromBytes bytes))
        (Url.percentDecode name)
        (slug
            |> String.replace "_" "/"
            |> String.replace "-" "+"
            |> Base64.toBytes
        )
        |> Maybe.withDefault Persona.default


personaToSlug : Persona -> String
personaToSlug persona =
    persona
        |> Persona.encode
        |> BitParser.bitsToBytes
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
            [ Persona.view
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
                    |> Drawing.drawTextCenter font (font.height + 2) (Persona.gendertropeToRecord persona.gendertrope).name
                    |> Drawing.drawText font 1 (font.height * 2 + 3) description
                    |> Drawing.drawText font 30 (font.height * 2 + 3) meters
                    |> Drawing.scaleBy (800 // cardImageSize.width)
                    |> Image.fromArray2d
                    |> Image.toPng
                    |> Response.bytesBody
                    |> Response.withHeader "Content-Type" "image/png"
            )
