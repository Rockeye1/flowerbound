module Site exposing (config, manifest)

import BackendTask exposing (BackendTask)
import Color
import FatalError exposing (FatalError)
import Head
import MimeType
import Pages.Manifest as Manifest
import Pages.Url
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
    , Head.appleTouchIcon (Just 180) (Pages.Url.fromPath [ "apple-touch-icon.png" ])
    , Head.icon [ ( 32, 32 ) ] MimeType.Png (Pages.Url.fromPath [ "favicon-32x32.png" ])
    , Head.icon [ ( 16, 16 ) ] MimeType.Png (Pages.Url.fromPath [ "favicon-16x16.png" ])
    , Head.metaName "theme-color" (Head.raw "#800080")
    , Head.manifestLink "/manifest.json"
    , Head.sitemapLink "/sitemap.xml"
    ]
        |> BackendTask.succeed


manifest : Manifest.Config
manifest =
    Manifest.init
        { name = "Flowerbound"
        , description = "An helper for the Flowerbound RPG"
        , startUrl = Route.Index |> Route.toPath
        , icons = [ androidChromeIcon 192, androidChromeIcon 512 ]
        }
        |> Manifest.withThemeColor (Color.rgb255 0x80 0 0x80)
        |> Manifest.withDisplayMode Manifest.Standalone


androidChromeIcon : Int -> Manifest.Icon
androidChromeIcon size =
    let
        sizeString : String
        sizeString =
            String.fromInt size
    in
    { src = Pages.Url.fromPath [ "android-chrome-" ++ sizeString ++ "x" ++ sizeString ++ ".png" ]
    , mimeType = Just MimeType.Png
    , sizes = [ ( size, size ) ]
    , purposes = []
    }


iconData :
    { src : Pages.Url.Url
    , mimeType : MimeType.MimeImage
    , sizes : List ( Int, Int )
    }
iconData =
    { src = Pages.Url.fromPath [ "/favicon.png" ]
    , mimeType = MimeType.Png
    , sizes = [ ( 1430, 1430 ) ]
    }
