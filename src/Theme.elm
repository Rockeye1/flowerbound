module Theme exposing (column, padding, rhythm, row, spacing)

import Element exposing (Attribute, Element)


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
