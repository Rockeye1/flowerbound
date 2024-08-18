module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Element
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import MimeType
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (StatelessRoute)
import Shared
import Site
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
        |> RouteBuilder.buildNoState
            { view = view
            }


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


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view _ _ =
    { title = Site.manifest.name
    , body = Element.text "TODO"
    }
