module Route.Persona.Slug_ exposing (ActionData, Data, Model, Msg, personaCodec, route)

import BackendTask exposing (BackendTask)
import Base64
import Effect exposing (Effect)
import Element exposing (el)
import ErrorPage exposing (ErrorPage(..))
import FatalError exposing (FatalError)
import Flate
import Head
import Head.Seo as Seo
import Image exposing (Image)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Route
import RouteBuilder exposing (App, StatefulRoute)
import Serialize as Codec exposing (Codec)
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
        |> Base64.toBytes
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen
            (\inflated ->
                inflated
                    |> Codec.decodeFromBytes personaCodec
                    |> Result.toMaybe
            )
        |> Maybe.withDefault defaultPersona


personaToSlug : Persona -> String
personaToSlug persona =
    persona
        |> Codec.encodeToBytes personaCodec
        |> Flate.deflate
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "/" "_"


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


personaCodec : Codec e Persona
personaCodec =
    Codec.record Persona
        |> Codec.field .name Codec.string
        |> Codec.field .fitness Codec.int
        |> Codec.field .grace Codec.int
        |> Codec.field .ardor Codec.int
        |> Codec.field .sanity Codec.int
        |> Codec.field .prowess Codec.int
        |> Codec.field .moxie Codec.int
        |> Codec.field .stamina Codec.int
        |> Codec.field .satiation Codec.int
        |> Codec.field .craving Codec.int
        |> Codec.field .arousal Codec.int
        |> Codec.field .sensitivity Codec.int
        |> Codec.field .euphoriaPoints Codec.int
        |> Codec.field .ichorPoints Codec.int
        |> Codec.field .numinousPoints Codec.int
        |> Codec.finishRecord


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
