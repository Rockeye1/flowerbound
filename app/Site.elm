module Site exposing (config, manifest)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Pages.Manifest as Manifest
import Route
import SiteConfig exposing (SiteConfig)


config : SiteConfig
config =
    { canonicalUrl = "http://localhost:1234"
    , head = head
    }


head : BackendTask FatalError (List Head.Tag)
head =
    [ Head.metaName "viewport" (Head.raw "width=device-width,initial-scale=1")
    , Head.sitemapLink "/sitemap.xml"
    ]
        |> BackendTask.succeed


manifest : Manifest.Config
manifest =
    Manifest.init
        { name = "Site Name"
        , description = "Description"
        , startUrl = Route.Index |> Route.toPath
        , icons = []
        }
