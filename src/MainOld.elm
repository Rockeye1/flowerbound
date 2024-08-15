module MainOld exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import List.Extra
import Persona
import Theme
import Types exposing (Flags, Model, Msg(..))
import Url


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
            ( { model | personas = List.Extra.updateAt index (\p -> { p | persona = persona }) model.personas }, Cmd.none )

        Flip index ->
            ( { model | personas = List.Extra.updateAt index (\p -> { p | flipped = not p.flipped }) model.personas }, Cmd.none )


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
        |> List.indexedMap
            (\index persona ->
                Persona.view
                    { update = ChangePersona index
                    , flip = Flip index
                    }
                    persona
            )
        |> Theme.column [ Theme.padding ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
