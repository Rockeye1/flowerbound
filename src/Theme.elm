module Theme exposing (button, column, el, link, padding, purple, rhythm, row, spacing, withHint, wrappedRow)

import Element exposing (Attribute, Element, el, link, shrink, width)
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
                    :: Font.color (Element.rgb 1 1 1)
                    :: Border.color (Element.rgb 0 0 0)
                    :: width (Element.minimum 38 shrink)
                    :: Font.center
                    :: attrs
                )
                config

        Nothing ->
            el
                (Border.width 1
                    :: padding
                    :: Background.color (Element.rgb 0.7 0.7 0.7)
                    :: width (Element.minimum 38 shrink)
                    :: Font.center
                    :: attrs
                )
                config.label


purple : Element.Color
purple =
    Element.rgb255 0x80 0 0x80


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
