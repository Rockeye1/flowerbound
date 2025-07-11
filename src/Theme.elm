module Theme exposing (Attribute, Context, Element, backgroundColorAccent, backgroundColorBackground, black, borderColorAccent, button, checkbox, column, desaturate, el, fontColorAccent, gray, iconAndTextButton, iconButton, input, lighten, link, multiline, padding, pageTitle, purple, rhythm, row, selectableButton, shadow, slider, spacing, style, title, toAccent, transparentLightGray, viewMarkdown, white, withHint, wrappedRow)

import Color
import Color.Oklch as Oklch
import Html
import Html.Attributes
import Icons
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Persona
import Phosphor
import Route
import Ui.Anim
import Ui.WithContext as Ui exposing (Color)
import Ui.WithContext.Accessibility as Accessibility
import Ui.WithContext.Font as Font
import Ui.WithContext.Input as Input
import Ui.WithContext.Prose as Prose
import Ui.WithContext.Shadow as Shadow


type alias Context =
    { colors : Persona.Colors
    , darkMode : Bool
    }


type alias Attribute msg =
    Ui.Attribute Context msg


type alias Element msg =
    Ui.Element Context msg


rhythm : number
rhythm =
    8


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Ui.column
        (spacing :: borderColorAccent :: attrs)
        children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Ui.row (spacing :: borderColorAccent :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Ui.row (spacing :: Ui.wrap :: borderColorAccent :: attrs) children


el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Ui.el (spacing :: borderColorAccent :: attrs) child


borderColorAccent : Attribute msg
borderColorAccent =
    Ui.fromContextAttribute (\{ colors } -> Ui.borderColor colors.accent)


fontColorAccent : Attribute msg
fontColorAccent =
    Ui.fromContextAttribute (\{ colors } -> Font.color colors.accent)


backgroundColorBackground : Attribute msg
backgroundColorBackground =
    Ui.fromContextAttribute (\{ colors } -> Ui.background colors.background)


backgroundColorAccent : Attribute msg
backgroundColorAccent =
    Ui.fromContextAttribute (\{ colors } -> Ui.background colors.accent)


spacing : Attribute msg
spacing =
    Ui.spacing rhythm


padding : Attribute msg
padding =
    Ui.padding rhythm


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs config =
    selectableButton attrs
        { onPress = config.onPress
        , label = config.label
        , selected = False
        }


selectableButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        , selected : Bool
        }
    -> Element msg
selectableButton attrs config =
    Ui.fromContext
        (\{ colors } ->
            let
                ( fg, bg, border ) =
                    case config.onPress of
                        Just _ ->
                            if config.selected then
                                ( white, colors.accent, black )

                            else
                                ( black, lighten colors.accent, colors.accent )

                        Nothing ->
                            ( black, gray, colors.accent )

                common : List (Attribute msg)
                common =
                    Ui.border 1
                        :: padding
                        :: Font.center
                        :: Ui.width Ui.shrink
                        :: Ui.widthMin 38
                        :: Ui.background bg
                        :: Font.color fg
                        :: Ui.borderColor border
                        :: attrs
            in
            Ui.el
                (case config.onPress of
                    Nothing ->
                        common

                    Just msg ->
                        Input.button msg
                            :: Ui.liftAttribute
                                (Ui.Anim.hovered (Ui.Anim.ms 100)
                                    [ Ui.Anim.backgroundColor
                                        (desaturate colors.accent
                                            |> Oklch.toColor
                                        )
                                    , Ui.Anim.fontColor Color.white
                                    ]
                                )
                            :: common
                )
                config.label
        )


transparentLightGray : Color
transparentLightGray =
    Oklch.oklcha 0.85 0 0 0.4


gray : Color
gray =
    Oklch.oklch 0.7 0 0


black : Color
black =
    Oklch.oklch 0 0 0


white : Color
white =
    Oklch.oklch 1 0 0


purple : Color
purple =
    Color.rgb255 0x80 0 0x80
        |> Oklch.fromColor


style : String -> String -> Attribute msg
style key value =
    Ui.htmlAttribute (Html.Attributes.style key value)


link :
    List (Attribute msg)
    ->
        { label : Element msg
        , route : Route.Route
        }
    -> Element msg
link attrs { label, route } =
    el
        (Ui.linkNewTab (Route.toString route)
            :: Font.underline
            :: attrs
        )
        label


input :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Input.Label
        }
    -> Element msg
input attrs config =
    Input.text
        (fontColorAccent :: attrs)
        config


multiline :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Input.Label
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs config =
    Input.multiline
        (fontColorAccent :: attrs)
        config


checkboxIcon : Bool -> Element msg
checkboxIcon checked =
    Ui.el
        [ Ui.htmlAttribute (Html.Attributes.class "focusable")
        , Ui.width (Ui.px 14)
        , Ui.height (Ui.px 14)
        , Font.color white
        , Ui.centerY
        , Font.size 9
        , Font.center
        , Ui.rounded 3
        , borderColorAccent
        , Shadow.shadows
            [ { x = 0
              , y = 0
              , blur = 1
              , size = 1
              , color =
                    if checked then
                        Color.rgba (238 / 255) (238 / 255) (238 / 255) 0
                            |> Oklch.fromColor

                    else
                        Color.rgb (238 / 255) (238 / 255) (238 / 255) |> Oklch.fromColor
              }
            ]
        , if checked then
            backgroundColorAccent

          else
            Ui.background white
        , Ui.border <|
            if checked then
                0

            else
                1
        , Ui.inFront
            (Ui.el
                [ Ui.borderColor white
                , Ui.height (Ui.px 6)
                , Ui.width (Ui.px 9)
                , Ui.rotate (Ui.radians (degrees -45))
                , Ui.centerX
                , Ui.centerY
                , Ui.move (Ui.up 1)
                , Ui.opacity
                    (if checked then
                        1

                     else
                        0
                    )
                , Ui.borderWith
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Ui.none
            )
        ]
        Ui.none


viewMarkdown : String -> List (Element msg)
viewMarkdown markdown =
    case Markdown.Parser.parse markdown of
        Ok blocks ->
            viewMarkdownBlocks blocks

        Err _ ->
            [ Ui.text "Could not parse Markdown" ]


viewMarkdownBlocks : List Markdown.Block.Block -> List (Element msg)
viewMarkdownBlocks blocks =
    case Markdown.Renderer.render markdownRenderer blocks of
        Ok elem ->
            elem

        Err e ->
            [ Ui.text e ]


markdownRenderer : Markdown.Renderer.Renderer (Element msg)
markdownRenderer =
    let
        viewParagraph : List (Element msg) -> Element msg
        viewParagraph =
            Prose.paragraph [ spacing ]

        viewBlockQuote : List (Element msg) -> Element msg
        viewBlockQuote =
            column [ padding, Ui.border 1 ]

        viewHtml : Markdown.Html.Renderer (List (Element msg) -> Element msg)
        viewHtml =
            Markdown.Html.oneOf []

        viewCodeSpan : String -> Element msg
        viewCodeSpan code =
            el [ Font.family [ Font.monospace ] ] (Ui.text code)

        viewStrong : List (Element msg) -> Element msg
        viewStrong =
            row [ Font.bold ]

        viewEmphasis : List (Element msg) -> Element msg
        viewEmphasis =
            row [ Font.italic ]

        viewStrikethrough : List (Element msg) -> Element msg
        viewStrikethrough =
            row [ Font.strike ]

        viewHardLineBreak : Element msg
        viewHardLineBreak =
            Html.br [] [] |> Ui.html

        viewLink : { title : Maybe String, destination : String } -> List (Element msg) -> Element msg
        viewLink =
            \data body ->
                el
                    [ maybeTitle data.title
                    , style "display" "inline-flex"
                    , Ui.linkNewTab data.destination
                    ]
                    (Prose.paragraph
                        [ fontColorAccent
                        , Font.underline
                        ]
                        body
                    )

        viewImage : { alt : String, src : String, title : Maybe String } -> Element msg
        viewImage image =
            Ui.image
                [ Ui.width Ui.fill
                , maybeTitle image.title
                ]
                { source = image.src
                , description = image.alt
                , onLoad = Nothing
                }

        viewUnorderedList : List (Markdown.Block.ListItem (Element msg)) -> Element msg
        viewUnorderedList items =
            items
                |> List.map viewUnorderedListItem
                |> column []

        viewOrderedList : Int -> List (List (Element msg)) -> Element msg
        viewOrderedList _ _ =
            Ui.text "TODO: orderedList"

        viewCodeBlock : { body : String, language : Maybe String } -> Element msg
        viewCodeBlock _ =
            Ui.text "TODO: codeBlock"

        viewThematicBreak : Element msg
        viewThematicBreak =
            Ui.none

        viewTable : List (Element msg) -> Element msg
        viewTable =
            Ui.column []

        viewTableHeader : List (Element msg) -> Element msg
        viewTableHeader =
            Ui.column
                [ Font.bold
                , Ui.width Ui.fill
                , Font.center
                ]

        viewTableBody : List (Element msg) -> Element msg
        viewTableBody =
            Ui.column [ Ui.width Ui.fill ]

        viewTableRow : List (Element msg) -> Element msg
        viewTableRow =
            Ui.row
                [ Ui.height Ui.fill
                , Ui.width Ui.fill
                ]

        viewTableCell : Maybe Markdown.Block.Alignment -> List (Element msg) -> Element msg
        viewTableCell =
            \maybeAlignment ->
                Prose.paragraph
                    (toAlignAttribute maybeAlignment
                        :: Ui.borderWith
                            { top = 1
                            , bottom = 0
                            , left = 0
                            , right = 0
                            }
                        :: tableBorder
                    )

        viewTableHeaderCell : Maybe Markdown.Block.Alignment -> List (Element msg) -> Element msg
        viewTableHeaderCell _ =
            Prose.paragraph tableBorder
    in
    { heading = viewHeading
    , paragraph = viewParagraph
    , blockQuote = viewBlockQuote
    , html = viewHtml
    , text = Ui.text
    , codeSpan = viewCodeSpan
    , strong = viewStrong
    , emphasis = viewEmphasis
    , strikethrough = viewStrikethrough
    , hardLineBreak = viewHardLineBreak
    , link = viewLink
    , image = viewImage
    , unorderedList = viewUnorderedList
    , orderedList = viewOrderedList
    , codeBlock = viewCodeBlock
    , thematicBreak = viewThematicBreak
    , table = viewTable
    , tableHeader = viewTableHeader
    , tableBody = viewTableBody
    , tableRow = viewTableRow
    , tableCell = viewTableCell
    , tableHeaderCell = viewTableHeaderCell
    }


toAlignAttribute : Maybe Markdown.Block.Alignment -> Attribute msg
toAlignAttribute alignment =
    case alignment of
        Nothing ->
            Ui.noAttr

        Just Markdown.Block.AlignLeft ->
            Font.alignLeft

        Just Markdown.Block.AlignRight ->
            Font.alignRight

        Just Markdown.Block.AlignCenter ->
            Font.center


tableBorder : List (Attribute msg)
tableBorder =
    [ Ui.borderColor (Color.rgb255 223 226 229 |> Oklch.fromColor)
    , Ui.paddingXY 6 13
    , Ui.height Ui.fill
    , Ui.width Ui.fill
    ]


viewHeading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
viewHeading { level, rawText, children } =
    Prose.paragraph
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
        , case level of
            Markdown.Block.H1 ->
                Accessibility.h1

            Markdown.Block.H2 ->
                Accessibility.h2

            Markdown.Block.H3 ->
                Accessibility.h3

            Markdown.Block.H4 ->
                Accessibility.h4

            Markdown.Block.H5 ->
                Accessibility.h5

            Markdown.Block.H6 ->
                Accessibility.h6
        , Ui.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Ui.htmlAttribute
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
        [ Ui.el [ Ui.alignTop ] (Ui.text (" " ++ mark ++ " "))
        , Prose.paragraph [] children
        ]


withHint : String -> Element msg -> Element msg
withHint hint label =
    el
        [ title hint
        , Font.underline
        , style "text-decoration-style" "dotted"
        , style "cursor" "help"
        , Ui.width Ui.shrink
        ]
        label


maybeTitle : Maybe String -> Attribute msg
maybeTitle mt =
    case mt of
        Just t ->
            title t

        Nothing ->
            Ui.noAttr


title : String -> Attribute msg
title hint =
    Ui.htmlAttribute <| Html.Attributes.title hint


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
    Ui.el [ Ui.paddingXY (rhythm * 2) rhythm ] <|
        Input.sliderHorizontal
            (Ui.height (Ui.px 30)
                :: Ui.behindContent
                    (Ui.el
                        [ Ui.width Ui.fill
                        , Ui.height (Ui.px 1)
                        , Ui.centerY
                        , backgroundColorAccent
                        , Ui.rounded 2
                        ]
                        Ui.none
                    )
                :: Ui.behindContent
                    (List.range config.min config.max
                        |> List.map
                            (\v ->
                                el
                                    [ Ui.width (Ui.px 1)
                                    , Ui.height (Ui.px 8)
                                    , Ui.borderWith { left = 1, right = 0, top = 0, bottom = 0 }
                                    , Ui.behindContent
                                        (Ui.el
                                            [ Ui.centerX
                                            , Ui.move (Ui.down 10)
                                            , Input.button (config.onChange v)
                                            ]
                                            (Ui.text (String.fromInt v))
                                        )
                                    ]
                                    Ui.none
                            )
                        |> List.intersperse (Ui.el [ Ui.width Ui.fill ] Ui.none)
                        |> Ui.row
                            [ Ui.width Ui.fill
                            , Ui.height Ui.fill
                            , spacing
                            ]
                    )
                :: Ui.move (Ui.up 8)
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
                    [ Ui.width (Ui.px 16)
                    , Ui.height (Ui.px 16)
                    , Ui.rounded 8
                    , Ui.border 1
                    , borderColorAccent
                    , Ui.fromContextAttribute (\{ colors } -> Ui.background (lighten colors.accent))
                    ]
                    |> Just
            }


pageTitle : String -> Element msg
pageTitle label =
    Ui.el
        [ Font.size 40
        , Font.bold
        , fontColorAccent
        , Font.center
        , Ui.width Ui.fill
        ]
        (Ui.text label)


iconButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , icon : Phosphor.IconVariant
        , title : String
        }
    -> Element msg
iconButton attrs config =
    button (title config.title :: attrs)
        { onPress = config.onPress
        , label = Icons.toElement config.icon
        }


iconAndTextButton :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , icon : Phosphor.IconVariant
        , label : String
        }
    -> Element msg
iconAndTextButton attrs config =
    button attrs
        { onPress = config.onPress
        , label =
            row
                [ Ui.width Ui.shrink ]
                [ Icons.toElement config.icon
                , Ui.text config.label
                ]
        }


checkbox :
    List (Attribute msg)
    ->
        { checked : Bool
        , onChange : Bool -> msg
        , label : Input.Label
        }
    -> Element msg
checkbox attrs config =
    Input.checkbox
        attrs
        { checked = config.checked
        , onChange = config.onChange
        , icon = Just (\b -> checkboxIcon b)
        , label = config.label
        }


desaturate : Color -> Color
desaturate color =
    { color | chroma = 0.5 * color.chroma }


lighten : Color -> Color
lighten color =
    { color | lightness = 0.98 }


toAccent : Color -> Color
toAccent color =
    { color | lightness = 0.42, chroma = 0.19 }


shadow : Attribute msg
shadow =
    Shadow.shadows
        [ { x = 0
          , y = 0
          , blur = 10
          , color = Oklch.oklcha 0 0 0 0.2
          , size = 2
          }
        ]
