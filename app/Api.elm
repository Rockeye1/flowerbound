module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Pages.Manifest as Manifest
import Persona.Codec
import Route exposing (Route)
import Route.Persona.Name_.Data__ as Persona
import Route.Religion
import Site
import Sitemap
import Url


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes getStaticRoutes {- htmlToString -} _ =
    [ ApiRoute.succeed
        (\name data _ ->
            case Persona.Codec.partialPersonaFromSlug data of
                Ok partialPersona ->
                    Persona.toCard (Maybe.withDefault name <| Url.percentDecode name) partialPersona

                Err e ->
                    BackendTask.fail (FatalError.fromString e)
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
