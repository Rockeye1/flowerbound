module Theme exposing (black, button, column, el, gray, input, lightGray, link, multiline, padding, purple, purpleCheckbox, purpleHex, rhythm, row, spacing, viewMarkdown, white, withHint, wrappedRow)

import Element exposing (Attribute, Element, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Route


rhythm : Int
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
                (Border.width 1
                    :: padding
                    :: Background.color purple
                    :: Font.center
                    :: Font.color white
                    :: Border.color black
                    :: width (Element.minimum 38 shrink)
                    :: attrs
                )
                config

        Nothing ->
            el
                (Border.width 1
                    :: padding
                    :: Font.center
                    :: Background.color gray
                    :: width (Element.minimum 38 shrink)
                    :: attrs
                )
                config.label


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


purpleHex : number
purpleHex =
    0x00800080


withHint : String -> Element msg -> Element msg
withHint hint label =
    el
        [ Element.htmlAttribute <| Html.Attributes.title hint
        , Font.underline
        , Element.htmlAttribute <| Html.Attributes.style "text-decoration-style" "dotted"
        , Element.htmlAttribute <| Html.Attributes.style "cursor" "help"
        ]
        label


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
    { heading = \_ -> Element.text "TODO: heading"
    , paragraph = Element.paragraph [ spacing ]
    , blockQuote =
        column
            [ padding
            , Border.width 1
            ]
    , html = Markdown.Html.oneOf []
    , text = Element.text
    , codeSpan = \_ -> Element.text "TODO: codeSpan"
    , strong = row [ Font.bold ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , hardLineBreak = Html.br [] [] |> Element.html
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ maybeTitle title
                , Element.htmlAttribute (Html.Attributes.style "display" "inline-flex")
                , Element.htmlAttribute (Html.Attributes.attribute "elm-pages:prefetch" "")
                ]
                { url = destination
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
    , table = \_ -> Element.text "TODO: table"
    , tableHeader = \_ -> Element.text "TODO: tableHeader"
    , tableBody = \_ -> Element.text "TODO: tableBody"
    , tableRow = \_ -> Element.text "TODO: tableRow"
    , tableCell = \_ _ -> Element.text "TODO: tableCell"
    , tableHeaderCell = \_ _ -> Element.text "TODO: tableHeaderCell"
    }


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


maybeTitle : Maybe String -> Attribute msg
maybeTitle title =
    case title of
        Just t ->
            Element.htmlAttribute (Html.Attributes.title t)

        Nothing ->
            Element.htmlAttribute (Html.Attributes.classList [])
