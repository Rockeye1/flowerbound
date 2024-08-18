module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import MimeType
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (StatelessRoute)
import Shared
import Site
import Theme
import UrlPath exposing (UrlPath)
import View exposing (View)


type alias Msg =
    ()


type alias Model =
    {}


type alias RouteParams =
    {}


type alias Data =
    {}


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


init : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect msg )
init _ _ =
    ( {}, Effect.none )


update : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect msg )
update _ _ msg model =
    ( model, Effect.none )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head _ =
    let
        image : Seo.Image
        image =
            { url = Pages.Url.fromPath [ "/android-chrome-192x192.png" ]
            , alt = "An orchid"
            , dimensions = Nothing
            , mimeType = Just (MimeType.Image MimeType.Png)
            }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.manifest.name
        , image = image
        , description = Site.manifest.description
        , locale = Nothing
        , title = Site.manifest.name
        }
        |> Seo.website


data : BackendTask FatalError Data
data =
    BackendTask.succeed {}


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg Msg)
view _ _ _ =
    { title = Site.manifest.name
    , body =
        Theme.column [ Theme.padding ]
            (Theme.viewMarkdown """
## Temperaments

**Innocent**: You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.
- Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Craving** value, drain the value of the result from your **Sensitivity**.

**Thoughtful**: You are dwelling on the emotions and emotional implications and the shape of your future.
- When calculating the Aftermath of your turn, first roll a **Moxie Check**. If the result is _less_ than your current **Arousal** value, drain the value of the result from your **Satiation**.

**Perverse**: You are excited on a conceptual, kinky level, captivated and compelled.
- Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Sensitivity** value, add the result to your **Craving** value.

**Valiant**: You are proud of yourself for enduring, but you are enduring rather than enjoying.
- When calculating whether or not you are currently having an **Orgasm**, roll a **Moxie Check**. If the result is _less_ than your current **Stamina** value, add the result to your Orgasm Threshold as a Modifier.

## Orgasm
To determine if you are Having An Orgasm you first determine your **Orgasm Threshold** by adding your **Sensitivity** to your **Satiation** and then also adding Modifiers if there are any.

ORGASM THRESHOLD = SENSITIVITY + SATIATION (+ MODIFIERS)

Once you know your **Orgasm Threshold**, you simply compare it to your **Arousal**. If your **Arousal** is greater than your **Orgasm Threshold**, you are **Having An Orgasm**.

`AROUSAL > ORGASM THRESHOLD`
""")
    }
