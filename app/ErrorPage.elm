module ErrorPage exposing (ErrorPage(..), Model, Msg, init, internalError, notFound, statusCode, update, view)

import Effect exposing (Effect)
import Ui exposing (text)
import Ui.Prose exposing (paragraph)
import View exposing (View)


type alias Msg =
    Never


type alias Model =
    {}


init : ErrorPage -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )


update : ErrorPage -> Msg -> Model -> ( Model, Effect Msg )
update _ _ model =
    ( model, Effect.none )


type ErrorPage
    = NotFound
    | InvalidRequest
    | InternalError String


notFound : ErrorPage
notFound =
    NotFound


internalError : String -> ErrorPage
internalError =
    InternalError


view : ErrorPage -> Model -> View Msg
view error _ =
    { body =
        paragraph []
            [ text <|
                case error of
                    NotFound ->
                        "Page not found. Maybe try another URL?"

                    InternalError string ->
                        "Something went wrong.\n" ++ string

                    InvalidRequest ->
                        "Invalid request."
            ]
    , title =
        case error of
            NotFound ->
                "Page Not Found"

            InternalError _ ->
                "Unexpected Error"

            InvalidRequest ->
                "Invalid Request"
    }


statusCode : ErrorPage -> number
statusCode error =
    case error of
        NotFound ->
            404

        InternalError _ ->
            500

        InvalidRequest ->
            400
