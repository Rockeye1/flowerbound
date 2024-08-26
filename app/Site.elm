module Site exposing (config, defaultSummary, image, manifest)

import BackendTask exposing (BackendTask)
import Color
import Element
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Hex
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
    { canonicalUrl =
        -- "http://localhost:1234"
        "https://uriel.tail1b193.ts.net"
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
    let
        colorCss : String
        colorCss =
            "#" ++ Hex.toString Theme.purpleHex
    in
    [ Head.metaName "viewport" (Head.raw "width=device-width,initial-scale=1")
    , Head.appleTouchIcon (Just 180) (Pages.Url.fromPath [ "apple-touch-icon.png" ])
    , Head.icon [ ( 32, 32 ) ] MimeType.Png (Pages.Url.fromPath [ "favicon-32x32.png" ])
    , Head.icon [ ( 16, 16 ) ] MimeType.Png (Pages.Url.fromPath [ "favicon-16x16.png" ])
    , Head.manifestLink "/manifest.json"
    , Head.nonLoadingNode "link"
        [ ( "rel", Head.raw "mask-icon" )
        , ( "href", Head.urlAttribute (Pages.Url.fromPath [ "safari-pinned-tab.svg" ]) )
        , ( "color", Head.raw colorCss )
        ]
    , Head.metaName "msapplication-TileColor" (Head.raw colorCss)
    , Head.metaName "theme-color" (Head.raw colorCss)
    , Head.sitemapLink "/sitemap.xml"
    ]
        |> BackendTask.succeed


manifest : Manifest.Config
manifest =
    Manifest.init
        { name = "Flowerbound"
        , description = "An helper for the Flowerbound RPG"
        , startUrl = Route.Index |> Route.toPath
        , icons = [ androidChromeIcon 192 ]
        }
        |> Manifest.withThemeColor (toColor Theme.purple)
        |> Manifest.withBackgroundColor (toColor Theme.purple)
        |> Manifest.withDisplayMode Manifest.Standalone


toColor : Element.Color -> Color.Color
toColor color =
    let
        rgb : { red : Float, green : Float, blue : Float, alpha : Float }
        rgb =
            Element.toRgb color
    in
    Color.rgba rgb.red rgb.blue rgb.blue rgb.alpha


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
