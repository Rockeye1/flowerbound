module ErrorPage exposing (ErrorPage(..), Model, Msg, init, internalError, notFound, statusCode, update, view)

import Effect exposing (Effect)
import Element exposing (paragraph, text)
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
    | InvalidInput
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

                    InvalidInput ->
                        "Invalid input."

                    InvalidRequest ->
                        "Invalid request."
            ]
    , title =
        case error of
            NotFound ->
                "Page Not Found"

            InternalError _ ->
                "Unexpected Error"

            InvalidInput ->
                "Invalid Input"

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

        InvalidInput ->
            400

        InvalidRequest ->
            400
