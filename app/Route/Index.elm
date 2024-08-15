module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import PagesMsg exposing (PagesMsg)
import Persona
import Route
import RouteBuilder exposing (StatelessRoute)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import View exposing (View)


type alias Msg =
    ()


type alias Model =
    {}


type alias RouteParams =
    {}


type alias Data =
    Never


type alias ActionData =
    Never


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
head app =
    never app.data


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data _ _ =
    BackendTask.succeed
        (Response.temporaryRedirect
            (Route.toString
                (Route.Persona__Name___Data__
                    { name = Persona.default.name
                    , data = Nothing
                    }
                )
            )
        )


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action =
    data


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view app _ =
    never app.data
