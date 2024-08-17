module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Pages.Manifest as Manifest
import Persona.Types exposing (Persona)
import Route exposing (Route)
import Route.Persona.Name_.Data__ as Persona
import Site
import Sitemap


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes getStaticRoutes {- htmlToString -} _ =
    [ ApiRoute.succeed
        (\name data _ ->
            let
                persona : Persona
                persona =
                    Persona.personaFromSlug name data
            in
            Persona.toCard persona
        )
        |> ApiRoute.literal "card"
        |> ApiRoute.slash
        |> ApiRoute.literal "persona"
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.serverRender
    , Manifest.generator
        Site.config.canonicalUrl
        (BackendTask.succeed Site.manifest)
    , ApiRoute.succeed
        (getStaticRoutes
            |> BackendTask.map
                (\staticRoutes ->
                    Sitemap.build { siteUrl = Site.config.canonicalUrl }
                        (List.map
                            (\route ->
                                { path = Route.toString route
                                , lastMod = Nothing
                                }
                            )
                            staticRoutes
                        )
                )
        )
        |> ApiRoute.literal "sitemap.xml"
        |> ApiRoute.single
    ]
