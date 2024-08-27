module Theme exposing (black, button, checkbox, column, el, gray, iconAndTextButton, iconButton, input, lightGray, lightPurple, link, multiline, padding, pageTitle, purple, purpleHex, rhythm, row, selectableButton, slider, spacing, style, table, title, viewMarkdown, white, withHint, wrappedRow)

import Color exposing (Color)
import Html
import Html.Attributes
import Icons
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Phosphor
import Route
import Ui exposing (Attribute, Element)
import Ui.Accessibility as Accessibility
import Ui.Font as Font
import Ui.Input as Input
import Ui.Prose as Prose
import Ui.Shadow as Shadow
import Ui.Table as Table


rhythm : number
rhythm =
    8


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Ui.column (spacing :: Ui.borderColor purple :: attrs) children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Ui.row (spacing :: Ui.borderColor purple :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Ui.row (spacing :: Ui.wrap :: Ui.borderColor purple :: attrs) children


el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Ui.el (spacing :: Ui.borderColor purple :: attrs) child


table :
    List (Attribute msg)
    -> Table.Config () () data msg
    -> List data
    -> Element msg
table attrs config data =
    Table.view (spacing :: Ui.borderColor purple :: attrs) config data


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
    el
        (case config.onPress of
            Just msg ->
                Input.button msg :: buttonAttrs white purple black attrs

            Nothing ->
                buttonAttrs black gray purple attrs
        )
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
    el
        (case config.onPress of
            Just msg ->
                if config.selected then
                    Input.button msg :: buttonAttrs white purple black attrs

                else
                    Input.button msg :: buttonAttrs black lightPurple purple attrs

            Nothing ->
                buttonAttrs black gray purple attrs
        )
        config.label


buttonAttrs : Color -> Color -> Color -> List (Attribute msg) -> List (Attribute msg)
buttonAttrs fg bg border attrs =
    Ui.border 1
        :: padding
        :: Font.center
        :: Ui.width Ui.shrink
        :: Ui.widthMin 38
        :: Ui.background bg
        :: Font.color fg
        :: Ui.borderColor border
        :: attrs


lightGray : Color
lightGray =
    Color.rgb 0.85 0.85 0.85


gray : Color
gray =
    Color.rgb 0.7 0.7 0.7


black : Color
black =
    Color.rgb 0 0 0


white : Color
white =
    Color.rgb 1 1 1


purple : Color
purple =
    Color.rgb255 0x80 0 0x80


lightPurple : Color
lightPurple =
    Color.rgb255 0xE6 0xCC 0xE6


purpleHex : number
purpleHex =
    0x00800080


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
    Route.toLink
        (\linkAttrs ->
            el
                (List.map Ui.htmlAttribute linkAttrs
                    ++ attrs
                )
                label
        )
        route


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
    Input.text (Font.color purple :: attrs) config


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
    Input.multiline (Font.color purple :: attrs) config


purpleCheckbox : Bool -> Element msg
purpleCheckbox checked =
    Ui.el
        [ Ui.htmlAttribute (Html.Attributes.class "focusable")
        , Ui.width
            (Ui.px 14)
        , Ui.height (Ui.px 14)
        , Font.color white
        , Ui.centerY
        , Font.size 9
        , Font.center
        , Ui.rounded 3
        , Ui.borderColor purple
        , Shadow.shadows
            [ { x = 0
              , y = 0
              , blur = 1
              , size = 1
              , color =
                    if checked then
                        Color.rgba (238 / 255) (238 / 255) (238 / 255) 0

                    else
                        Color.rgb (238 / 255) (238 / 255) (238 / 255)
              }
            ]
        , Ui.background <|
            if checked then
                purple

            else
                white
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
    { heading = viewHeading
    , paragraph = Prose.paragraph [ spacing ]
    , blockQuote =
        column
            [ padding
            , Ui.border 1
            ]
    , html = Markdown.Html.oneOf []
    , text = Ui.text
    , codeSpan = \code -> el [ Font.family [ Font.monospace ] ] (Ui.text code)
    , strong = row [ Font.bold ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , hardLineBreak = Html.br [] [] |> Ui.html
    , link =
        \data body ->
            el
                [ maybeTitle data.title
                , style "display" "inline-flex"
                , Ui.linkNewTab data.destination
                ]
                (Prose.paragraph
                    [ Font.color (Color.rgb255 0 0 255)
                    ]
                    body
                )
    , image =
        \image ->
            Ui.image
                [ Ui.width Ui.fill
                , maybeTitle image.title
                ]
                { source = image.src
                , description = image.alt
                , onLoad = Nothing
                }
    , unorderedList =
        \items ->
            items
                |> List.map viewUnorderedListItem
                |> column []
    , orderedList = \_ _ -> Ui.text "TODO: orderedList"
    , codeBlock = \_ -> Ui.text "TODO: codeBlock"
    , thematicBreak = Ui.none
    , table = Ui.column []
    , tableHeader =
        Ui.column
            [ Font.bold
            , Ui.width Ui.fill
            , Font.center
            ]
    , tableBody = Ui.column [ Ui.width Ui.fill ]
    , tableRow =
        Ui.row
            [ Ui.height Ui.fill
            , Ui.width Ui.fill
            ]
    , tableCell =
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
    , tableHeaderCell = \_ -> Prose.paragraph tableBorder
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
    [ Ui.borderColor (Color.rgb255 223 226 229)
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
    el [ Ui.paddingXY (rhythm * 2) rhythm ] <|
        Input.sliderHorizontal
            (Ui.height (Ui.px 30)
                :: Ui.behindContent
                    (Ui.el
                        [ Ui.width Ui.fill
                        , Ui.height (Ui.px 1)
                        , Ui.centerY
                        , Ui.background purple
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
                                        (el
                                            [ Ui.centerX
                                            , Ui.move (Ui.down 4)
                                            ]
                                            (Ui.text (String.fromInt v))
                                        )
                                    ]
                                    Ui.none
                            )
                        |> List.intersperse (el [ Ui.width Ui.fill ] Ui.none)
                        |> row
                            [ Ui.width Ui.fill
                            , Ui.height Ui.fill
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
                    , Ui.borderColor purple
                    , Ui.background lightPurple
                    ]
                    |> Just
            }


pageTitle : String -> Element msg
pageTitle label =
    Ui.el
        [ Font.size 40
        , Font.bold
        , Font.color purple
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
            row []
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
    Input.checkbox attrs
        { checked = config.checked
        , onChange = config.onChange
        , icon = Just purpleCheckbox
        , label = config.label
        }
