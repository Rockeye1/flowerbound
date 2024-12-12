module Site exposing (config, defaultSummary, manifest)

import BackendTask exposing (BackendTask)
import Color.Oklch as Oklch
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import LanguageTag.Language
import LanguageTag.Region
import MimeType
import Pages.Manifest as Manifest
import Pages.Url
import Route
import SiteConfig exposing (SiteConfig)
import Theme


config : SiteConfig
config =
    { canonicalUrl = "https://flowerbound.netlify.app"
    , head = head
    }


defaultSummary :
    { canonicalUrlOverride : Maybe String
    , siteName : String
    , image : Seo.Image
    , description : String
    , locale : Maybe ( LanguageTag.Language.Language, LanguageTag.Region.Region )
    , title : String
    }
defaultSummary =
    { canonicalUrlOverride = Nothing
    , siteName = manifest.name
    , image = image
    , description = manifest.description
    , locale = Nothing
    , title = manifest.name
    }


head : BackendTask FatalError (List Head.Tag)
head =
    [ Head.metaName "viewport" (Head.raw "width=device-width,initial-scale=1")
    , Head.icon [ ( 48, 48 ) ] MimeType.Png (Pages.Url.fromPath [ "favicon-48x48.png" ])
    , Head.icon [] (MimeType.OtherImage "svg+xml") (Pages.Url.fromPath [ "favicon.svg" ])
    , Head.icon [] (MimeType.OtherImage "x-icon") (Pages.Url.fromPath [ "favicon.ico" ])
    , Head.appleTouchIcon (Just 180) (Pages.Url.fromPath [ "apple-touch-icon.png" ])
    , Head.metaName "apple-mobile-web-app-title" (Head.raw manifest.name)
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
        , icons =
            [ androidChromeIcon 192
            , androidChromeIcon 512
            ]
        }
        |> Manifest.withShortName "Flowerbound"
        |> Manifest.withThemeColor (Theme.purple |> Oklch.toColor)
        |> Manifest.withBackgroundColor (Theme.purple |> Oklch.toColor)
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
    , purposes = [ Manifest.IconPurposeMaskable ]
    }


image : { url : Pages.Url.Url, alt : String, dimensions : Maybe { width : Int, height : Int }, mimeType : Maybe MimeType.MimeType }
image =
    let
        icon =
            androidChromeIcon 192
    in
    { url = icon.src
    , alt = "An orchid"
    , dimensions = Just { width = 192, height = 192 }
    , mimeType = Maybe.map MimeType.Image icon.mimeType
    }
