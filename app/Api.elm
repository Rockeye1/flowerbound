module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Image
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


type alias Image =
    Array (Array Image.Pixel)


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
                        in
                        image
                            |> drawImage 1 1 flower
                            |> drawImage (Persona.cardImageSize.width - 5 - 1) 1 flower
                            |> drawTextCenter font 1 persona.name
                            |> drawText font 1 (font.height + 2) (Persona.toDescription persona)
                            |> scaleBy 4
                            |> Image.fromArray2d
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


drawTextCenter : Font -> Int -> String -> Image -> Image
drawTextCenter font y text img =
    let
        imageWidth : Int
        imageWidth =
            img
                |> Array.get 0
                |> Maybe.withDefault Array.empty
                |> Array.length

        textWidth : Int
        textWidth =
            (font.width + 1) * String.length text - 1
    in
    drawTextNoWrap font ((imageWidth - textWidth) // 2) y text img


flower : Image
flower =
    let
        red =
            0xF04030FF

        brick =
            0xC02020FF

        yellow =
            0xFFF000FF

        transparent =
            0
    in
    [ [ transparent, red, red, red, transparent ]
    , [ red, brick, red, brick, red ]
    , [ red, red, yellow, red, red ]
    , [ red, brick, red, brick, red ]
    , [ transparent, red, red, red, transparent ]
    ]
        |> List.map Array.fromList
        |> Array.fromList


scaleBy : Int -> Image -> Image
scaleBy factor img =
    let
        pushN : Int -> a -> Array a -> Array a
        pushN k e acc =
            if k <= 0 then
                acc

            else
                pushN (k - 1) e (Array.push e acc)
    in
    Array.foldl
        (\row acc ->
            let
                scaledRow : Array Int
                scaledRow =
                    Array.foldl (\pixel rowAcc -> pushN factor pixel rowAcc) Array.empty row
            in
            acc |> pushN factor scaledRow
        )
        Array.empty
        img


drawText : Font -> Int -> Int -> String -> Image -> Image
drawText font x y rawText image =
    let
        imageWidth : Int
        imageWidth =
            image
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
        (\char ( currentX, imageAcc ) ->
            ( currentX + font.width + 1
            , drawChar font currentX y char imageAcc
            )
        )
        ( x, image )
        text
        |> Tuple.second


drawChar : Font -> Int -> Int -> Char -> Image -> Image
drawChar font x y char img =
    case Dict.get (Char.toUpper char) font.chars of
        Nothing ->
            img

        Just toDraw ->
            drawImage x y toDraw img


drawImage : Int -> Int -> Image -> Image -> Image
drawImage x y toDraw img =
    let
        width : Int
        width =
            toDraw
                |> Array.get 0
                |> Maybe.withDefault Array.empty
                |> Array.length

        height : Int
        height =
            Array.length toDraw
    in
    List.foldl
        (\dy imgAcc ->
            case Array.get (y + dy) imgAcc of
                Nothing ->
                    imgAcc

                Just row ->
                    case Array.get dy toDraw of
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
                                                    Array.set (x + dx) px rowAcc
                                        )
                                        row
                                        (List.range 0 (width - 1))
                            in
                            Array.set (y + dy) newRow imgAcc
        )
        img
        (List.range 0 (height - 1))


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
                    |> List.map2 (\char pixels -> ( char, Array.fromList (List.map Array.fromList pixels) ))
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
