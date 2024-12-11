module Shared exposing (Data, Model, Msg(..), template)

import BackendTask exposing (BackendTask)
import Browser.Events
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Persona
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Ui
import Ui.Anim
import Ui.Font as Font
import Ui.WithContext
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
    | Anim Ui.Anim.Msg


type alias Data =
    ()


type alias Model =
    { flipped : Bool
    , width : Int
    , height : Int
    , anim : Ui.Anim.State
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
      , anim = Ui.Anim.init
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

        Anim animMsg ->
            let
                ( newAnim, cmd ) =
                    Ui.Anim.update Anim animMsg model.anim
            in
            ( { model | anim = newAnim }, Effect.fromCmd cmd )


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
view _ _ shared toMsg pageView =
    { body =
        [ Html.node "style" [] [ Html.text """::backdrop {
  backdrop-filter: blur(3px);
}

div.popover {
  border: none;
  background: none;
  flex-wrap: wrap;
  gap: 8px;
  justify-content: center;
}

div.popover:popover-open {
  display: flex;
}

div.popover button {
  font-family: "Atkinson Hyperlegible";
  display: flex;
  gap: 8px;
  align-items: center;
  padding: 8px;
  color: black;
  font-size: 20px;
  border: 1px solid black;
  background-color: var(--accent-color);
  cursor: pointer;
}

div.popover button:hover {
  color: white;
  background-color: var(--hover-color);
}""" ]
        , Ui.Anim.layout
            { options = []
            , toMsg = \msg -> toMsg (Anim msg)
            , breakpoints = Nothing
            }
            shared.anim
            [ -- Ui.focusStyle
              -- { backgroundColor = Nothing
              -- , borderColor = Nothing
              -- , shadow =
              --     Just
              --         { color = Theme.purple
              --         , offset = ( 0, 0 )
              --         , blur = 0
              --         , size = 3
              --         }
              -- },
              Ui.height Ui.fill
            , Font.family
                [ Font.typeface "Atkinson Hyperlegible"
                , Font.sansSerif
                ]
            , Font.size 20
            ]
            (Ui.WithContext.withContext
                { colors = Persona.toColors { hue = Nothing }
                , darkMode = True
                }
                pageView.body
            )
        ]
    , title = pageView.title
    }
