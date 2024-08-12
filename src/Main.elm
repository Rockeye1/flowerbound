module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import List.Extra
import Theme
import Types exposing (Flags, Model, Msg(..), Persona)
import Url
import View.Persona


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , personas =
            [ cinderellaSheen
            ]
      }
    , Cmd.none
    )


cinderellaSheen : Persona
cinderellaSheen =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ChangePersona index persona ->
            ( { model | personas = List.Extra.setAt index persona model.personas }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Element.layout [] (innerView model)
        ]
    }


innerView : Model -> Element Msg
innerView { personas } =
    personas
        |> List.indexedMap (\index persona -> Element.map (ChangePersona index) (View.Persona.view persona))
        |> Theme.column [ Theme.padding ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
