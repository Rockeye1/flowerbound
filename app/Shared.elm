module Shared exposing (Data, Model, Msg(..), template)

import BackendTask exposing (BackendTask)
import Browser.Events
import Effect exposing (Effect)
import Element
import FatalError exposing (FatalError)
import Html exposing (Html)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Theme
import UrlPath exposing (UrlPath)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


type Msg
    = Flip
    | Resized Int Int


type alias Data =
    ()


type alias Model =
    { flipped : Bool
    , width : Int
    , height : Int
    }


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init _ _ =
    ( { flipped = False
      , width = 800
      , height = 600
      }
    , Effect.MeasureScreen Resized
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Flip ->
            ( { model | flipped = not model.flipped }, Effect.none )

        Resized w h ->
            ( { model
                | width = w
                , height = h
              }
            , Effect.none
            )


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onResize Resized


data : BackendTask FatalError Data
data =
    BackendTask.succeed ()


view :
    Data
    ->
        { path : UrlPath
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html msg), title : String }
view _ _ _ _ pageView =
    { body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { backgroundColor = Nothing
                    , borderColor = Nothing
                    , shadow =
                        Just
                            { color = Theme.purple
                            , offset = ( 0, 0 )
                            , blur = 0
                            , size = 3
                            }
                    }
                ]
            }
            []
            pageView.body
        ]
    , title = pageView.title
    }
