module Theme exposing (button, column, padding, rhythm, row, spacing)

import Element exposing (Attribute, Element, el, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


rhythm : Int
rhythm =
    8


column :
    List (Attribute msg)
    -> List (Element msg)
    -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


row :
    List (Attribute msg)
    -> List (Element msg)
    -> Element msg
row attrs children =
    Element.row (spacing :: attrs) children


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
                    :: Background.color (Element.rgb 0.9 0.6 0.9)
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
