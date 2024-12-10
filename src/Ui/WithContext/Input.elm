module Ui.WithContext.Input exposing (Label, button, checkbox, label, labelHidden, multiline, sliderHorizontal, sliderVertical, text, thumb)

import Ui as OrigUi
import Ui.Input
import Ui.WithContext as Ui exposing (Attribute, Element)


type alias Label =
    Ui.Input.Label


type Thumb context msg
    = Thumb (context -> Ui.Input.Thumb msg)


button : msg -> Attribute context msg
button msg =
    Ui.liftAttribute (Ui.Input.button msg)


text :
    List (Attribute context msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element context msg
text attrs config =
    Ui.withAttrs Ui.Input.text attrs config


multiline :
    List (Attribute context msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        , spellcheck : Bool
        }
    -> Element context msg
multiline attrs config =
    Ui.withAttrs Ui.Input.multiline attrs config


checkbox :
    List (Attribute context msg)
    ->
        { onChange : Bool -> msg
        , icon : Maybe (Bool -> Element context msg)
        , checked : Bool
        , label : Label
        }
    -> Element context msg
checkbox attrs config =
    Ui.fromContext
        (\context ->
            Ui.withAttrs Ui.Input.checkbox
                attrs
                { onChange = config.onChange
                , checked = config.checked
                , icon =
                    Maybe.map
                        (\toIcon v -> Ui.withContext context (toIcon v))
                        config.icon
                , label = config.label
                }
        )


sliderHorizontal :
    List (Attribute context msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Thumb context msg)
        , step : Maybe Float
        }
    -> Element context msg
sliderHorizontal attrs config =
    Ui.fromContext
        (\context ->
            Ui.withAttrs Ui.Input.sliderHorizontal
                attrs
                { label = config.label
                , onChange = config.onChange
                , min = config.min
                , max = config.max
                , value = config.value
                , step = config.step
                , thumb = Maybe.map (\(Thumb t) -> t context) config.thumb
                }
        )


sliderVertical :
    List (Attribute context msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Thumb context msg)
        , step : Maybe Float
        }
    -> Element context msg
sliderVertical attrs config =
    Ui.fromContext
        (\context ->
            Ui.withAttrs Ui.Input.sliderVertical
                attrs
                { label = config.label
                , onChange = config.onChange
                , min = config.min
                , max = config.max
                , value = config.value
                , step = config.step
                , thumb = Maybe.map (\(Thumb t) -> t context) config.thumb
                }
        )


thumb : List (Attribute context msg) -> Thumb context msg
thumb attrs =
    Thumb
        (\context ->
            Ui.Input.thumb
                (List.map
                    (\attr -> Ui.withContextAttribute context attr)
                    attrs
                )
        )


labelHidden : String -> Label
labelHidden value =
    Ui.Input.labelHidden value


label :
    String
    -> List (Attribute context msg)
    -> Element context msg
    ->
        { element : Element context msg
        , id : Ui.Input.Label
        }
label id attrs element =
    { id = (Ui.Input.label id [] OrigUi.none).id
    , element =
        Ui.withChild
            (\oattrs child -> (Ui.Input.label id oattrs child).element)
            attrs
            element
    }
