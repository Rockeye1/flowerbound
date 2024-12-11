module Ui.WithContext exposing (Angle, Attribute, Color, Edges, Element, Length, Position, alignBottom, alignRight, alignTop, background, behindContent, below, border, borderColor, borderWith, centerX, centerY, colorToCss, column, down, el, fill, fromContext, fromContextAttribute, height, html, htmlAttribute, image, inFront, left, liftAttribute, liftElement, linkNewTab, map, move, noAttr, none, opacity, padding, paddingWith, paddingXY, px, radians, replaceContext, right, rotate, rounded, row, scrollableX, shrink, spacing, text, turns, up, updateContext, width, widthMax, widthMin, withAttrs, withChild, withChildren, withContext, withContextAttribute, wrap)

import Color.Oklch as Oklch
import Html
import Html.Attributes
import Ui


type Element context msg
    = Element (context -> Ui.Element msg)


type Attribute context msg
    = Attribute (context -> Ui.Attribute msg)


type alias Color =
    Oklch.Oklch


type alias Length =
    Ui.Length


type alias Angle =
    Ui.Angle


type alias Edges =
    Ui.Edges


type alias Position =
    Ui.Position


liftElement : Ui.Element msg -> Element context msg
liftElement element =
    Element (\_ -> element)


liftAttribute : Ui.Attribute msg -> Attribute context msg
liftAttribute attr =
    Attribute (\_ -> attr)


withContext : context -> Element context msg -> Ui.Element msg
withContext context (Element toElem) =
    toElem context


updateContext : (context -> context) -> Element context msg -> Element context msg
updateContext f (Element toElem) =
    Element (\context -> toElem (f context))


replaceContext : context -> Element context msg -> Element c msg
replaceContext context (Element toElem) =
    Element (\_ -> toElem context)


withContextAttribute : context -> Attribute context msg -> Ui.Attribute msg
withContextAttribute context (Attribute toAttribute) =
    toAttribute context


fromContext : (context -> Element context msg) -> Element context msg
fromContext toElem =
    Element
        (\context ->
            let
                (Element inner) =
                    toElem context
            in
            inner context
        )


fromContextAttribute : (context -> Attribute context msg) -> Attribute context msg
fromContextAttribute toAttr =
    Attribute
        (\context ->
            let
                (Attribute inner) =
                    toAttr context
            in
            inner context
        )


withAttrs :
    (List (Ui.Attribute msg) -> config -> Ui.Element msg)
    -> List (Attribute context msg)
    -> config
    -> Element context msg
withAttrs toNode attrs child =
    Element
        (\context ->
            toNode
                (List.map (\(Attribute attr) -> attr context) attrs)
                child
        )


withChild :
    (List (Ui.Attribute msg) -> Ui.Element msg -> Ui.Element msg)
    -> List (Attribute context msg)
    -> Element context msg
    -> Element context msg
withChild toNode attrs (Element child) =
    Element
        (\context ->
            toNode
                (List.map (\(Attribute attr) -> attr context) attrs)
                (child context)
        )


withChildren :
    (List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg)
    -> List (Attribute context msg)
    -> List (Element context msg)
    -> Element context msg
withChildren toNode attrs children =
    Element
        (\context ->
            toNode
                (List.map (\(Attribute attr) -> attr context) attrs)
                (List.map (\(Element child) -> child context) children)
        )



-- Lifted


text : String -> Element context msg
text content =
    liftElement (Ui.text content)


background : Color -> Attribute context msg
background color =
    htmlAttribute (Html.Attributes.style "background-color" (colorToCss color))


borderColor : Color -> Attribute context msg
borderColor color =
    htmlAttribute (Html.Attributes.style "border-color" (colorToCss color))


colorToCss : Color -> String
colorToCss color =
    "oklch("
        ++ String.fromFloat color.lightness
        ++ " "
        ++ String.fromFloat color.chroma
        ++ " "
        ++ String.fromFloat (360 * color.hue)
        ++ ")"


borderWith : Edges -> Attribute context msg
borderWith value =
    liftAttribute (Ui.borderWith value)


paddingWith : Edges -> Attribute context msg
paddingWith value =
    liftAttribute (Ui.paddingWith value)


spacing : Int -> Attribute context msg
spacing value =
    liftAttribute (Ui.spacing value)


padding : Int -> Attribute context msg
padding value =
    liftAttribute (Ui.padding value)


border : Int -> Attribute context msg
border value =
    liftAttribute (Ui.border value)


wrap : Attribute context msg
wrap =
    liftAttribute Ui.wrap


row : List (Attribute context msg) -> List (Element context msg) -> Element context msg
row =
    withChildren Ui.row


column : List (Attribute context msg) -> List (Element context msg) -> Element context msg
column =
    withChildren Ui.column


el : List (Attribute context msg) -> Element context msg -> Element context msg
el =
    withChild Ui.el


width : Length -> Attribute context msg
width value =
    liftAttribute (Ui.width value)


height : Length -> Attribute context msg
height value =
    liftAttribute (Ui.height value)


fill : Length
fill =
    Ui.fill


shrink : Length
shrink =
    Ui.shrink


px : Int -> Length
px value =
    Ui.px value


scrollableX : Attribute context msg
scrollableX =
    liftAttribute Ui.scrollableX


widthMin : Int -> Attribute context msg
widthMin value =
    liftAttribute (Ui.widthMin value)


widthMax : Int -> Attribute context msg
widthMax value =
    liftAttribute (Ui.widthMax value)


html : Html.Html msg -> Element context msg
html child =
    liftElement (Ui.html child)


htmlAttribute : Html.Attribute msg -> Attribute context msg
htmlAttribute attr =
    liftAttribute (Ui.htmlAttribute attr)


linkNewTab : String -> Attribute context msg
linkNewTab target =
    liftAttribute (Ui.linkNewTab target)


alignBottom : Attribute context msg
alignBottom =
    liftAttribute Ui.alignBottom


alignRight : Attribute context msg
alignRight =
    liftAttribute Ui.alignRight


alignTop : Attribute context msg
alignTop =
    liftAttribute Ui.alignTop


centerX : Attribute context msg
centerX =
    liftAttribute Ui.centerX


centerY : Attribute context msg
centerY =
    liftAttribute Ui.centerY


opacity : Float -> Attribute context msg
opacity value =
    liftAttribute (Ui.opacity value)


move : Position -> Attribute context msg
move value =
    liftAttribute (Ui.move value)


up : Int -> Position
up value =
    Ui.up value


down : Int -> Position
down value =
    Ui.down value


right : Int -> Position
right value =
    Ui.right value


left : Int -> Position
left value =
    Ui.left value


map : (a -> b) -> Element context a -> Element context b
map f (Element elem) =
    Element (\context -> Ui.map f (elem context))


noAttr : Attribute context msg
noAttr =
    liftAttribute Ui.noAttr


rounded : Int -> Attribute context msg
rounded value =
    liftAttribute (Ui.rounded value)


none : Element context msg
none =
    liftElement Ui.none


paddingXY : Int -> Int -> Attribute context msg
paddingXY x y =
    liftAttribute (Ui.paddingXY x y)


behindContent : Element context msg -> Attribute context msg
behindContent (Element child) =
    Attribute (\context -> Ui.behindContent (child context))


inFront : Element context msg -> Attribute context msg
inFront (Element child) =
    Attribute (\context -> Ui.inFront (child context))


below : Element context msg -> Attribute context msg
below (Element child) =
    Attribute (\context -> Ui.below (child context))


image :
    List (Attribute context msg)
    ->
        { source : String
        , description : String
        , onLoad : Maybe msg
        }
    -> Element context msg
image attrs config =
    withAttrs Ui.image attrs config


rotate : Angle -> Attribute context msg
rotate value =
    liftAttribute (Ui.rotate value)


radians : Float -> Angle
radians value =
    Ui.radians value


turns : Float -> Angle
turns value =
    Ui.turns value
