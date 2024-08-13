module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import BackendTask.File
import FatalError exposing (FatalError)
import Html exposing (Html)
import Image exposing (Image)
import Pages.Manifest as Manifest
import Route exposing (Route)
import Server.Response as Response


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes getStaticRoutes htmlToString =
    [ ApiRoute.succeed
        (\persona _ ->
            let
                image : Image
                image =
                    0xFF0000FF
                        |> List.repeat 200
                        |> List.repeat 300
                        |> Image.fromList2d
            in
            image
                |> Image.toPng
                |> Response.bytesBody
                |> BackendTask.succeed
        )
        |> ApiRoute.literal "persona"
        |> ApiRoute.slash
        |> ApiRoute.literal "image"
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.serverRender
    ]


manifest : Manifest.Config
manifest =
    Manifest.init
        { name = "Site Name"
        , description = "Description"
        , startUrl = Route.Index |> Route.toPath
        , icons = []
        }
