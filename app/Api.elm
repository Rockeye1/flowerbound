module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Image exposing (Image)
import List.Extra
import Pages.Manifest as Manifest
import Parser exposing ((|.), (|=), Parser)
import Route exposing (Route)
import Route.Persona.Name_.Data__ as Persona
import Server.Response as Response
import Site
import Sitemap
import String.Extra
import Theme
import Types exposing (Persona)


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
            getFont
                |> BackendTask.andThen
                    (\font ->
                        let
                            image : Image
                            image =
                                (Theme.purpleHex * 256 + 0xFF)
                                    |> Array.repeat Persona.cardImageSize.width
                                    |> Array.repeat Persona.cardImageSize.height
                                    |> Image.fromArray2d
                        in
                        image
                            |> drawText font 1 1 persona.name
                            |> drawText font 1 (font.height + 2) (Persona.toDescription persona)
                            |> scaleBy 4
                            |> Image.toPng
                            |> Response.bytesBody
                            |> Response.withHeader "Content-Type" "image/png"
                            |> BackendTask.succeed
                    )
        )
        |> ApiRoute.literal "persona"
        |> ApiRoute.slash
        |> ApiRoute.literal "image"
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


scaleBy : Int -> Image -> Image
scaleBy factor img =
    let
        pushN k e acc =
            if k <= 0 then
                acc

            else
                pushN (k - 1) e (Array.push e acc)
    in
    img
        |> Image.toArray2d
        |> Array.foldl
            (\row acc ->
                let
                    scaledRow : Array Int
                    scaledRow =
                        Array.foldl (\pixel rowAcc -> pushN factor pixel rowAcc) Array.empty row
                in
                acc |> pushN factor scaledRow
            )
            Array.empty
        |> Image.fromArray2d


drawText : Font -> Int -> Int -> String -> Image -> Image
drawText font x y rawText image =
    let
        imageWidth =
            image
                |> Image.toArray2d
                |> Array.get 0
                |> Maybe.withDefault Array.empty
                |> Array.length

        maxWidth : Int
        maxWidth =
            (imageWidth - x) // (font.width + 1)

        lines : List String
        lines =
            rawText
                |> String.split "\n"
                |> List.concatMap
                    (\line ->
                        line
                            |> String.Extra.softBreak maxWidth
                            |> List.map String.trim
                    )
    in
    lines
        |> List.foldl
            (\line ( currentY, acc ) ->
                ( currentY + font.height + 1
                , drawTextNoWrap font x currentY line acc
                )
            )
            ( y, image )
        |> Tuple.second


drawTextNoWrap : Font -> Int -> Int -> String -> Image -> Image
drawTextNoWrap font x y text image =
    String.foldl
        (\char ( currentX, img ) ->
            case Dict.get (Char.toUpper char) font.chars of
                Nothing ->
                    ( currentX + font.width + 1, img )

                Just charImg ->
                    let
                        charArray =
                            Image.toArray2d charImg

                        newImg =
                            List.foldl
                                (\dy imgAcc ->
                                    case Array.get (y + dy) imgAcc of
                                        Nothing ->
                                            imgAcc

                                        Just row ->
                                            case Array.get dy charArray of
                                                Nothing ->
                                                    imgAcc

                                                Just charRow ->
                                                    let
                                                        newRow =
                                                            List.foldl
                                                                (\dx rowAcc ->
                                                                    case Array.get dx charRow of
                                                                        Nothing ->
                                                                            rowAcc

                                                                        Just 0 ->
                                                                            rowAcc

                                                                        Just px ->
                                                                            Array.set (currentX + dx) px rowAcc
                                                                )
                                                                row
                                                                (List.range 0 (font.width - 1))
                                                    in
                                                    Array.set (y + dy) newRow imgAcc
                                )
                                img
                                (List.range 0 (font.height - 1))
                    in
                    ( currentX + font.width + 1, newImg )
        )
        ( x, Image.toArray2d image )
        text
        |> Tuple.second
        |> Image.fromArray2d


type alias Font =
    { width : Int
    , height : Int
    , chars : Dict Char Image
    }


getFont : BackendTask FatalError Font
getFont =
    File.rawFile "dist/microfont.pbm"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\rawFile ->
                let
                    range : Char -> Char -> List Char
                    range from to =
                        List.range
                            (Char.toCode from)
                            (Char.toCode to)
                            |> List.map Char.fromCode
                in
                Parser.run
                    (fontParser (range 'A' 'Z' ++ range '0' '9'))
                    rawFile
                    |> Result.mapError (\e -> FatalError.fromString (Debug.toString e))
                    |> BackendTask.fromResult
            )


fontParser : List Char -> Parser Font
fontParser characters =
    Parser.succeed
        (\width height data ->
            let
                fontWidth : Int
                fontWidth =
                    width // List.length characters
            in
            { width = fontWidth
            , height = height
            , chars =
                data
                    |> List.map
                        (\p ->
                            if p == 1 then
                                0xFFFFFFFF

                            else
                                0
                        )
                    |> List.Extra.greedyGroupsOf width
                    |> List.Extra.transpose
                    |> List.Extra.greedyGroupsOf fontWidth
                    |> List.map List.Extra.transpose
                    |> List.map2 (\char pixels -> ( char, Image.fromList2d pixels ))
                        characters
                    |> Dict.fromList
            }
        )
        |. Parser.symbol "P1"
        |. Parser.spaces
        -- width
        |= Parser.int
        |. Parser.spaces
        -- height
        |= Parser.int
        |. Parser.spaces
        -- pixels
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = Parser.int
            , trailing = Parser.Optional
            }
