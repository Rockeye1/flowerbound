module Theme exposing (button, column, el, gray, input, link, multiline, padding, purple, purpleHex, rhythm, row, spacing, withHint, wrappedRow)

import Element exposing (Attribute, Element, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
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
                    :: Font.color (Element.rgb 1 1 1)
                    :: Border.color (Element.rgb 0 0 0)
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


gray : Element.Color
gray =
    Element.rgb 0.7 0.7 0.7


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
