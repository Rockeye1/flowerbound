module Route.Persona.Name_.Data__ exposing (ActionData, Data, Model, Msg, RouteParams, route, toCard)

import Array
import BackendTask exposing (BackendTask)
import Color
import Color.Oklch as Oklch
import Drawing
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import File exposing (File)
import File.Download
import Head
import Head.Seo as Seo
import Image
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Editor
import RouteBuilder exposing (App, StatefulRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import Site
import Theme
import Types exposing (GendertropeRecord, PartialPersona, Persona)
import Ui.WithContext as Ui
import Url
import UrlPath exposing (UrlPath)
import View exposing (View)


type alias Model =
    Persona


type Msg
    = Flip
    | Upload
    | Update Persona
    | Picked File
    | Loaded (Result String Persona)
    | Download


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
                |> Maybe.andThen Persona.Codec.fragmentToGendertropeRecord
    in
    ( Persona.fromPartial name partialPersona maybeGendertrope
    , Effect.none
    )


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg, Maybe Shared.Msg )
update _ _ msg model =
    case msg of
        Update persona ->
            setPersona persona

        Flip ->
            ( model, Effect.none, Just Shared.Flip )

        Upload ->
            ( model
            , Effect.PickMarkdown Picked
            , Nothing
            )

        Picked file ->
            ( model
            , Effect.ReadPersonaFromMarkdown file Loaded
            , Nothing
            )

        Loaded (Ok persona) ->
            setPersona persona

        Loaded (Err _) ->
            -- TODO
            -- let
            --     _ =
            --         Debug.log "Error loading file" e
            -- in
            ( model, Effect.none, Nothing )

        Download ->
            ( model
            , Effect.fromCmd
                (File.Download.string
                    (model.name ++ ".md")
                    "text/markdown"
                    (Persona.Codec.toString model)
                )
            , Nothing
            )


setPersona : Persona -> ( Model, Effect Msg, Maybe Shared.Msg )
setPersona persona =
    ( persona
    , Effect.SetRouteToPersona persona
    , Nothing
    )


type alias Data =
    ( String, Maybe PartialPersona )


type alias ActionData =
    {}


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data params _ =
    ( Url.percentDecode params.name
        |> Maybe.withDefault params.name
    , Maybe.andThen
        (\d ->
            d
                |> Persona.Codec.partialPersonaFromSlug
                |> Result.toMaybe
        )
        params.data
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
            , Persona.Codec.partialPersonaToSlug persona
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
        Persona.Editor.view
            { update = Update
            , flip = Flip
            , upload = Upload
            , download = Download
            }
            { persona = model
            , flipped = shared.flipped
            }
            |> Theme.el
                [ Theme.padding
                , Theme.backgroundColorBackground
                , Ui.height Ui.fill
                ]
            |> Ui.updateContext (\context -> { context | colors = Persona.toColors model })
            |> Ui.map PagesMsg.fromMsg
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
                        let
                            color : { red : Float, green : Float, blue : Float, alpha : Float }
                            color =
                                Theme.purple
                                    |> Oklch.toColor
                                    |> Color.toRgba

                            f : Float -> Int -> Int
                            f val scale =
                                floor (val * 255) * 2 ^ scale
                        in
                        (f color.red 24 + f color.green 16 + f color.blue 8 + f 1 0)
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
