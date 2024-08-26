module Theme exposing (black, button, column, el, gray, input, lightGray, lightPurple, link, multiline, noAttribute, padding, pageTitle, purple, purpleCheckbox, purpleHex, rhythm, row, selectableButton, slider, spacing, style, table, title, viewMarkdown, white, withHint, wrappedRow)

import Element exposing (Attribute, Element, paddingXY)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Route


rhythm : number
rhythm =
    8


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: Border.color purple :: attrs) children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (spacing :: Border.color purple :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Element.wrappedRow (spacing :: Border.color purple :: attrs) children


el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Element.el (spacing :: Border.color purple :: attrs) child


table :
    List (Attribute msg)
    -> { data : List records, columns : List (Element.Column records msg) }
    -> Element msg
table attrs child =
    Element.table (spacing :: Border.color purple :: attrs) child


spacing : Attribute msg
spacing =
    Element.spacing rhythm


padding : Attribute msg
padding =
    Element.padding rhythm


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs config =
    case config.onPress of
        Just _ ->
            Input.button
                (buttonAttrs white purple black attrs)
                config

        Nothing ->
            el
                (buttonAttrs black gray purple attrs)
                config.label


selectableButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        , selected : Bool
        }
    -> Element msg
selectableButton attrs config =
    case config.onPress of
        Just _ ->
            Input.button
                (if config.selected then
                    buttonAttrs white purple black attrs

                 else
                    buttonAttrs black lightPurple purple attrs
                )
                { label = config.label
                , onPress = config.onPress
                }

        Nothing ->
            el
                (buttonAttrs black gray purple attrs)
                config.label


buttonAttrs : Element.Color -> Element.Color -> Element.Color -> List (Attribute msg) -> List (Attribute msg)
buttonAttrs fg bg border attrs =
    Border.width 1
        :: padding
        :: Font.center
        :: Element.width (Element.minimum 38 Element.shrink)
        :: Background.color bg
        :: Font.color fg
        :: Border.color border
        :: attrs


lightGray : Element.Color
lightGray =
    Element.rgb 0.85 0.85 0.85


gray : Element.Color
gray =
    Element.rgb 0.7 0.7 0.7


black : Element.Color
black =
    Element.rgb 0 0 0


white : Element.Color
white =
    Element.rgb 1 1 1


purple : Element.Color
purple =
    Element.rgb255 0x80 0 0x80


lightPurple : Element.Color
lightPurple =
    Element.rgb255 0xE6 0xCC 0xE6


purpleHex : number
purpleHex =
    0x00800080


style : String -> String -> Attribute msg
style key value =
    Element.htmlAttribute (Html.Attributes.style key value)


link :
    List (Attribute msg)
    ->
        { label : Element msg
        , route : Route.Route
        }
    -> Element msg
link attrs { label, route } =
    Element.link
        (Element.htmlAttribute (Html.Attributes.attribute "elm-pages:prefetch" "")
            :: attrs
        )
        { url = Route.toString route
        , label = label
        }


input :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
input attrs config =
    Input.text (Font.color purple :: attrs) config


multiline :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs config =
    Input.multiline (Font.color purple :: attrs) config


purpleCheckbox : Bool -> Element msg
purpleCheckbox checked =
    Element.el
        [ Element.htmlAttribute (Html.Attributes.class "focusable")
        , Element.width
            (Element.px 14)
        , Element.height (Element.px 14)
        , Font.color white
        , Element.centerY
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color purple
        , Border.shadow
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Background.color <|
            if checked then
                purple

            else
                white
        , Border.width <|
            if checked then
                0

            else
                1
        , Element.inFront
            (Element.el
                [ Border.color white
                , Element.height (Element.px 6)
                , Element.width (Element.px 9)
                , Element.rotate (degrees -45)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 1
                , Element.transparent (not checked)
                , Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none
            )
        ]
        Element.none


viewMarkdown : String -> List (Element msg)
viewMarkdown markdown =
    case Markdown.Parser.parse markdown of
        Ok blocks ->
            viewMarkdownBlocks blocks

        Err _ ->
            [ Element.text "Could not parse Markdown" ]


viewMarkdownBlocks : List Markdown.Block.Block -> List (Element msg)
viewMarkdownBlocks blocks =
    case Markdown.Renderer.render markdownRenderer blocks of
        Ok elem ->
            elem

        Err e ->
            [ Element.text e ]


markdownRenderer : Markdown.Renderer.Renderer (Element msg)
markdownRenderer =
    { heading = viewHeading
    , paragraph = Element.paragraph [ spacing ]
    , blockQuote =
        column
            [ padding
            , Border.width 1
            ]
    , html = Markdown.Html.oneOf []
    , text = Element.text
    , codeSpan = \code -> el [ Font.family [ Font.monospace ] ] (Element.text code)
    , strong = row [ Font.bold ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , hardLineBreak = Html.br [] [] |> Element.html
    , link =
        \data body ->
            Element.newTabLink
                [ maybeTitle data.title
                , style "display" "inline-flex"
                , Element.htmlAttribute (Html.Attributes.attribute "elm-pages:prefetch" "")
                ]
                { url = data.destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , image =
        \image ->
            Element.image
                [ Element.width Element.fill
                , maybeTitle image.title
                ]
                { src = image.src, description = image.alt }
    , unorderedList =
        \items ->
            items
                |> List.map viewUnorderedListItem
                |> column []
    , orderedList = \_ _ -> Element.text "TODO: orderedList"
    , codeBlock = \_ -> Element.text "TODO: codeBlock"
    , thematicBreak = Element.none
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column [ Element.width Element.fill ]
    , tableRow =
        Element.row
            [ Element.height Element.fill
            , Element.width Element.fill
            ]
    , tableCell =
        \maybeAlignment ->
            Element.paragraph
                (toAlignAttribute maybeAlignment
                    :: Border.widthEach
                        { top = 1
                        , bottom = 0
                        , left = 0
                        , right = 0
                        }
                    :: tableBorder
                )
    , tableHeaderCell = \_ -> Element.paragraph tableBorder
    }


toAlignAttribute : Maybe Markdown.Block.Alignment -> Attribute msg
toAlignAttribute alignment =
    case alignment of
        Nothing ->
            noAttribute

        Just Markdown.Block.AlignLeft ->
            Font.alignLeft

        Just Markdown.Block.AlignRight ->
            Font.alignRight

        Just Markdown.Block.AlignCenter ->
            Font.center


tableBorder : List (Attribute msg)
tableBorder =
    [ Border.color (Element.rgb255 223 226 229)
    , Element.paddingXY 6 13
    , Element.height Element.fill
    , Element.width Element.fill
    ]


viewHeading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
viewHeading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Markdown.Block.H1 ->
                    36

                Markdown.Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Element.Region.heading (Markdown.Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


viewUnorderedListItem : Markdown.Block.ListItem (Element msg) -> Element msg
viewUnorderedListItem (Markdown.Block.ListItem task children) =
    let
        mark : String
        mark =
            case task of
                Markdown.Block.NoTask ->
                    "-"

                Markdown.Block.IncompleteTask ->
                    "[ ]"

                Markdown.Block.CompletedTask ->
                    "[V]"
    in
    row []
        [ Element.el [ Element.alignTop ] (Element.text (" " ++ mark ++ " "))
        , Element.paragraph [] children
        ]


withHint : String -> Element msg -> Element msg
withHint hint label =
    el
        [ title hint
        , Font.underline
        , style "text-decoration-style" "dotted"
        , style "cursor" "help"
        ]
        label


maybeTitle : Maybe String -> Attribute msg
maybeTitle mt =
    case mt of
        Just t ->
            title t

        Nothing ->
            noAttribute


title : String -> Attribute msg
title hint =
    Element.htmlAttribute <| Html.Attributes.title hint


noAttribute : Attribute msg
noAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


slider :
    List (Attribute msg)
    ->
        { min : Int
        , max : Int
        , onChange : Int -> msg
        , label : String
        , value : Int
        }
    -> Element msg
slider attrs config =
    el [ padding ] <|
        Input.slider
            (Element.height (Element.px 30)
                :: Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 1)
                        , Element.centerY
                        , Background.color purple
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                :: Element.behindContent
                    (List.range config.min config.max
                        |> List.map
                            (\v ->
                                el
                                    [ Element.width (Element.px 1)
                                    , Element.height (Element.px 8)
                                    , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                                    , Element.behindContent
                                        (el
                                            [ Element.centerX
                                            , Element.moveDown 11
                                            ]
                                            (Element.text (String.fromInt v))
                                        )
                                    ]
                                    Element.none
                            )
                        |> List.intersperse (el [ Element.width Element.fill ] Element.none)
                        |> row
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , paddingXY 8 0
                            ]
                    )
                :: Element.moveUp 8
                :: attrs
            )
            { onChange = \v -> config.onChange (round v)
            , label = Input.labelHidden config.label
            , min = toFloat config.min
            , max = toFloat config.max
            , step = Nothing
            , value = toFloat config.value
            , thumb =
                Input.thumb
                    [ Element.width (Element.px 16)
                    , Element.height (Element.px 16)
                    , Border.rounded 8
                    , Border.width 1
                    , Border.color purple
                    , Background.color lightPurple
                    ]
            }


pageTitle : String -> Element msg
pageTitle label =
    Element.el
        [ Font.size 40
        , Font.bold
        , Font.color purple
        , Font.center
        , Element.width Element.fill
        ]
        (Element.text label)
