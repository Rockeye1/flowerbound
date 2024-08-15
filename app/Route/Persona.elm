module Route.Persona exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Element
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Route
import Route.Persona.Name_.Data__
import RouteBuilder exposing (StatelessRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
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
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = action
        }
        |> RouteBuilder.buildNoState
            { view = view
            }


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.manifest.name
        , image =
            { url = Pages.Url.external "/"
            , alt = "Card"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = ""
        , locale = Nothing
        , title = "Persona"
        }
        |> Seo.website


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data _ _ =
    BackendTask.succeed
        (Response.temporaryRedirect
            (Route.toString
                (Route.Persona__Name___Data__
                    { name = Route.Persona.Name_.Data__.defaultPersona.name
                    , data = Nothing
                    }
                )
            )
        )


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action _ _ =
    BackendTask.succeed (Response.errorPage (ErrorPage.InternalError "Unexpected call to Route.Persona.action"))


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view _ _ =
    { title = "Persona"
    , body = Element.text "..."
    }
