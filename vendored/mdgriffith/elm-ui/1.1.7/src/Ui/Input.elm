module Ui.Input exposing
    ( Label, label, labelHidden
    , checkbox
    , button
    , text, multiline
    , username, newPassword, currentPassword, email, search, spellChecked
    , sliderHorizontal, sliderVertical, Thumb, thumb, thumbWith
    , chooseOne, Option, option, optionWith, OptionState(..)
    )

{-| Input elements have a lot of constraints!

We want all of our input elements to:

  - _Always be accessible_
  - _Behave intuitively_
  - _Be completely restyleable_

While these three goals may seem pretty obvious, Html and CSS have made it surprisingly difficult to achieve!

And incredibly difficult for developers to remember all the tricks necessary to make things work. If you've every tried to make a `<textarea>` be the height of it's content or restyle a radio button while maintaining accessibility, you may be familiar.

This module is intended to be accessible by default. You shouldn't have to wade through docs, articles, and books to find out [exactly how accessible your html actually is](https://www.powermapper.com/tests/screen-readers/aria/index.html).


# Labels

Every input has a required `Label`.

@docs Label, label, labelHidden


# Checkboxes

A checkbox requires you to store a `Bool` in your model.

This is also the first input element that has a [`required label`](#Label).

    import Ui exposing (text)
    import Ui.Input

    type Msg
        = GuacamoleChecked Bool

    view model =
        Ui.Input.checkbox []
            { onChange = GuacamoleChecked
            , icon = Nothing -- We will get a default icon
            , checked = model.guacamole
            , label =
                Ui.Input.labelRight []
                    (text "Do you want Guacamole?")
            }

@docs checkbox


# Button

@docs button


# Text

@docs text, multiline


## Text with autofill

If we want to play nicely with a browser's ability to autofill a form, we need to be able to give it a hint about what we're expecting.

The following inputs are very similar to `Input.text`, but they give the browser a hint to allow autofill to work correctly.

@docs username, newPassword, currentPassword, email, search, spellChecked


# Sliders

A slider is great for choosing between a range of numerical values.

  - **thumb** - The icon that you click and drag to change the value.
  - **track** - The line behind the thumb denoting where you can slide to.

@docs sliderHorizontal, sliderVertical, Thumb, thumb, thumbWith


# Choose One or a 'Radio' Selection

    Input.chooseOne Ui.row
        [ padding 10
        , spacing 20
        ]
        { onChange = ChooseLunch
        , selected = Just model.lunch
        , label = Input.labelAbove [] (text "Lunch")
        , options =
            [ Input.option Burrito (text "Burrito")
            , Input.option Taco (text "Taco!")
            , Input.option Gyro (text "Gyro")
            ]
        }

**Note** we're using `Input.option`, which will render a default icon you're probably used to. If you want compeltely custom styling, use `Input.optionWith`!

@docs chooseOne, Option, option, optionWith, OptionState


# Form Elements

You might be wondering where something like `<form>` is.

What I've found is that most people who want `<form>` usually want it for the [implicit submission behavior](https://html.spec.whatwg.org/multipage/form-control-infrastructure.html#implicit-submission) or to be clearer, they want to do something when the `Enter` key is pressed.

Instead of implicit submission behavior, [try making an `onEnter` event handler like in this Ellie Example](https://ellie-app.com/5X6jBKtxzdpa1). Then everything is explicit!

And no one has to look up obtuse html documentation to understand the behavior of their code :).


# File Inputs

Presently, elm-ui does not expose a replacement for `<input type="file">`; in the meantime, an `Input.button` and `elm/file`'s `File.Select` may meet your needs.


# Disabling Inputs

You also might be wondering how to disable an input.

Disabled inputs can be a little problematic for user experience, and doubly so for accessibility. This is because it's now your priority to inform the user _why_ some field is disabled.

If an input is truly disabled, meaning it's not focusable or doesn't send off a `Msg`, you actually lose your ability to help the user out! For those wary about accessibility [this is a big problem.](https://ux.stackexchange.com/questions/103239/should-disabled-elements-be-focusable-for-accessibility-purposes)

Here are some alternatives to think about that don't involve explicitly disabling an input.

**Disabled Buttons** - Change the `Msg` it fires, the text that is rendered, and optionally set a `Region.description` which will be available to screen readers.

    import Ui.Input as Input
    import Ui.Region as Region

    myButton ready =
        if ready then
            Input.button
                [ Background.color blue
                ]
                { onPress =
                    Just SaveButtonPressed
                , label =
                    text "Save blog post"
                }

        else
            Input.button
                [ Background.color grey
                , Region.description
                    "A publish date is required before saving a blogpost."
                ]
                { onPress =
                    Just DisabledSaveButtonPressed
                , label =
                    text "Save Blog "
                }

Consider showing a hint if `DisabledSaveButtonPressed` is sent.

For other inputs such as `Input.text`, consider simply rendering it in a normal `paragraph` or `el` if it's not editable.

Alternatively, see if it's reasonable to _not_ display an input if you'd normally disable it. Is there an option where it's only visible when it's editable?

-}

import Html
import Html.Attributes
import Html.Events
import Internal.BitField as BitField
import Internal.Flag as Flag
import Internal.Model2 as Two
import Internal.Style.Generated exposing (classes)
import Internal.Style2 as Style
import Json.Decode as Json
import Ui exposing (Attribute, Element)
import Ui.Accessibility
import Ui.Events
import Ui.Font
import Ui.Shadow


white : Ui.Color
white =
    Ui.rgb 255 255 255


blue : Ui.Color
blue =
    Ui.rgb 59 153 252


darkGrey2 : Ui.Color
darkGrey2 =
    Ui.rgb 186 189 182


charcoal : Ui.Color
charcoal =
    Ui.rgb
        136
        138
        133


{-| -}
type Label
    = HiddenLabel String
    | LabelFromId String


{-| This is very similar to `Ui.Events.onClick`, but will change the underlying HTML to use a `<button>` element.

This is important for accessibility because it allows the button to be keyboard focused and triggered by the `Enter` key as well as the `Space` key.

This is an attribute instead of an element so that it can easily be switched out for a link using `Ui.link`.

-}
button : msg -> Ui.Attribute msg
button =
    Two.onPress


{-| Sometimes you may need to have a label which is not visible, but is still accessible to screen readers.

Seriously consider a visible label before using this.

The situations where a hidden label makes sense:

  - A searchbar with a `search` button right next to it.
  - A `table` of inputs where the header gives the label.

Basically, a hidden label works when there are other contextual clues that sighted people can pick up on.

-}
labelHidden : String -> Label
labelHidden =
    HiddenLabel


{-|

    let
        label =
            Ui.Input.label "guac-checked"
                []
                (Ui.text "Do you want Guacamole?")
    in
    Ui.row []
        [ Ui.Input.checkbox []
            { onChange = GuacamoleChecked
            , icon = Nothing
            , checked = model.guacamole

            -- Hand the label id to the checkbox
            , label = label.id
            }
        , label.element
        ]

-}
label :
    String
    -> List (Attribute msg)
    -> Element msg
    ->
        { element : Element msg
        , id : Label
        }
label id attrs labelElement =
    { element =
        Two.element Two.NodeAsLabel
            Two.AsColumn
            (Ui.htmlAttribute (Html.Attributes.for id)
                :: Ui.width Ui.fill
                :: attrs
            )
            [ labelElement ]
    , id = LabelFromId id
    }


labelHtmlAttribute : Label -> Html.Attribute a
labelHtmlAttribute lbl =
    case lbl of
        HiddenLabel textLabel ->
            Html.Attributes.attribute "aria-label" textLabel

        LabelFromId id ->
            Html.Attributes.id id


labelAttribute : Label -> Ui.Attribute a
labelAttribute lbl =
    case lbl of
        HiddenLabel textLabel ->
            Ui.Accessibility.description textLabel

        LabelFromId id ->
            Ui.id id


{-| -}
checkbox :
    List (Attribute msg)
    ->
        { onChange : Bool -> msg
        , icon : Maybe (Bool -> Element msg)
        , checked : Bool
        , label : Label
        }
    -> Element msg
checkbox attrs options =
    let
        input =
            Two.element Two.NodeAsInput
                Two.AsEl
                ([ Two.attribute
                    (Html.Attributes.type_ "checkbox")
                 , Two.attribute <|
                    Html.Attributes.attribute "aria-checked" <|
                        if options.checked then
                            "true"

                        else
                            "false"
                 , Two.attribute (Html.Attributes.class classes.inputReset)
                 , labelAttribute options.label
                 , Two.attribute (Html.Events.onClick (options.onChange (not options.checked)))
                 , onKeyLookup2 <|
                    \code ->
                        if code == enter then
                            Just <| options.onChange (not options.checked)

                        else if code == space then
                            Just <| options.onChange (not options.checked)

                        else
                            Nothing
                 , Two.attribute (Html.Attributes.tabindex 0)
                 , Ui.pointer
                 , Ui.width Ui.fill
                 , Ui.height Ui.fill
                 ]
                    ++ attrs
                )
                []

        icon =
            let
                viewIcon =
                    Maybe.withDefault defaultCheckbox options.icon
            in
            viewIcon options.checked
    in
    Ui.el
        [ Ui.inFront input
        , Ui.width Ui.shrink
        , Ui.height Ui.shrink
        ]
        icon


{-| -}
type Thumb msg
    = Thumb
        (Direction
         ->
            { thumb : List (Attribute msg)
            , below : List (Attribute msg)
            , above : List (Attribute msg)
            }
        )


{-| -}
thumb : List (Attribute msg) -> Thumb msg
thumb attrs =
    Thumb
        (\_ ->
            { thumb = attrs
            , below = []
            , above = []
            }
        )


{-| You can specify the style of the thumb and also the track that the thumb slides on.

    - `lowerTrack` - Is the part of the track that is on the left or below the thumb.  This one is commonly highlighted with a special color.
    - `higherTrack` - Is the part of the track that is on the right or above the thumb.  This

-}
thumbWith :
    { thumb : List (Attribute msg)
    , lowerTrack : List (Attribute msg)
    , higherTrack : List (Attribute msg)
    }
    -> Thumb msg
thumbWith details =
    Thumb
        (\_ ->
            { thumb = details.thumb
            , below = details.lowerTrack
            , above = details.higherTrack
            }
        )


{-| -}
defaultThumb : Thumb msg
defaultThumb =
    Thumb
        (\direction ->
            { thumb =
                [ Ui.width (Ui.px 16)
                , Ui.height (Ui.px 16)
                , Ui.rounded 8
                , Ui.border 1
                , Ui.borderColor charcoal
                , Ui.background white
                ]
            , above =
                case direction of
                    Horizontal ->
                        [ Ui.height (Ui.px 8)
                        , Ui.roundedWith
                            { topLeft = 0
                            , topRight = 8
                            , bottomLeft = 0
                            , bottomRight = 8
                            }
                        , Ui.border 1
                        , Ui.borderColor charcoal
                        , Ui.background white
                        , Ui.widthMin 8
                        ]

                    Vertical ->
                        [ Ui.width (Ui.px 8)
                        , Ui.height Ui.fill
                        , Ui.roundedWith
                            { topLeft = 8
                            , topRight = 8
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Ui.border 1
                        , Ui.centerX
                        , Ui.borderColor charcoal
                        , Ui.background white
                        , Ui.heightMin 8
                        ]
            , below =
                case direction of
                    Horizontal ->
                        [ Ui.height (Ui.px 8)
                        , Ui.roundedWith
                            { topLeft = 8
                            , topRight = 0
                            , bottomLeft = 8
                            , bottomRight = 0
                            }
                        , Ui.border 1
                        , Ui.borderColor charcoal
                        , Ui.background blue
                        , Ui.widthMin 8
                        ]

                    Vertical ->
                        [ Ui.height Ui.fill
                        , Ui.width (Ui.px 8)
                        , Ui.centerX
                        , Ui.roundedWith
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 8
                            , bottomRight = 8
                            }
                        , Ui.border 1
                        , Ui.borderColor (Ui.rgb 100 100 100)
                        , Ui.background blue
                        , Ui.heightMin 8
                        ]
            }
        )


type Direction
    = Horizontal
    | Vertical


{-| -}
sliderHorizontal :
    List (Ui.Attribute msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Thumb msg)
        , step : Maybe Float
        }
    -> Element msg
sliderHorizontal attributes input =
    viewSlider attributes input Horizontal


{-| -}
sliderVertical :
    List (Ui.Attribute msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Thumb msg)
        , step : Maybe Float
        }
    -> Element msg
sliderVertical attributes input =
    viewSlider attributes input Vertical


viewSlider :
    List (Ui.Attribute msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Thumb msg)
        , step : Maybe Float
        }
    -> Direction
    -> Element msg
viewSlider attributes input direction =
    let
        realThumb =
            Maybe.withDefault defaultThumb input.thumb

        isVertical =
            case direction of
                Horizontal ->
                    False

                Vertical ->
                    True

        factor =
            (input.value - input.min)
                / (input.max - input.min)
    in
    Ui.el
        ([ Ui.behindContent
            (viewThumb factor realThumb direction)
         , case direction of
            Horizontal ->
                Ui.width Ui.fill

            Vertical ->
                Ui.width (Ui.px 20)
         , case direction of
            Horizontal ->
                Ui.height (Ui.px 20)

            Vertical ->
                Ui.height Ui.fill
         ]
            ++ attributes
        )
        (Ui.html <|
            Html.input
                [ labelHtmlAttribute input.label
                , Html.Attributes.class Style.classes.slider
                , attrIf isVertical (Html.Attributes.attribute "orient" "vertical")
                , attrIf isVertical (Html.Attributes.attribute "writing-mode" "bt-lr")
                , attrIf isVertical (Html.Attributes.style "appearance" "slider-vertical")
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "width" "100%"
                , Html.Events.onInput
                    (\str ->
                        case String.toFloat str of
                            Nothing ->
                                -- This should never happen because the browser
                                -- should always provide a Float.
                                input.onChange 0

                            Just val ->
                                input.onChange val
                    )
                , Html.Attributes.type_ "range"
                , Html.Attributes.step
                    (case input.step of
                        Nothing ->
                            -- Note: If we set `any` here,
                            -- Firefox makes a single press of the arrows keys equal to 1
                            -- We could set the step manually to the effective range / 100
                            -- String.fromFloat ((input.max - input.min) / 100)
                            -- Which matches Chrome's default behavior
                            -- HOWEVER, that means manually moving a slider with the mouse will snap to that interval.
                            "any"

                        Just step ->
                            String.fromFloat step
                    )
                , Html.Attributes.min (String.fromFloat input.min)
                , Html.Attributes.max (String.fromFloat input.max)
                , Html.Attributes.value (String.fromFloat input.value)
                ]
                []
        )


attrIf : Bool -> Html.Attribute a -> Html.Attribute a
attrIf condition attr =
    if condition then
        attr

    else
        Html.Attributes.class ""


viewThumb : Float -> Thumb msg -> Direction -> Element msg
viewThumb factor (Thumb toThumbAttrs) direction =
    let
        thumbAttributes =
            toThumbAttrs direction

        layout =
            case direction of
                Horizontal ->
                    Ui.row

                Vertical ->
                    Ui.column

        alignment =
            case direction of
                Horizontal ->
                    Ui.centerY

                Vertical ->
                    Ui.centerX

        below =
            Ui.el
                (Two.style
                    "flex-grow"
                    (String.fromInt (round (factor * 5000)))
                    :: thumbAttributes.below
                )
                Ui.none

        above =
            Ui.el
                (Two.style
                    "flex-grow"
                    (String.fromInt (round ((1 - factor) * 5000)))
                    :: thumbAttributes.above
                )
                Ui.none
    in
    layout
        [ Ui.width Ui.fill
        , Ui.height Ui.fill
        , alignment
        , Two.attribute (Html.Attributes.style "pointer-events" "none")
        ]
        [ case direction of
            Horizontal ->
                below

            Vertical ->
                above
        , Ui.el
            [ alignment
            , case direction of
                Horizontal ->
                    Ui.width (Ui.px 0)

                Vertical ->
                    Ui.height (Ui.px 0)
            , Ui.inFront
                (Ui.el
                    [ Ui.contentCenterY
                    , Ui.contentCenterX
                    , Ui.width (Ui.px 0)
                    , Ui.height (Ui.px 0)
                    ]
                    (Ui.el thumbAttributes.thumb
                        Ui.none
                    )
                )
            ]
            Ui.none
        , case direction of
            Horizontal ->
                above

            Vertical ->
                below
        ]


type alias TextInput =
    { type_ : TextKind
    , spellchecked : Bool
    , autofill : Maybe String
    }


type TextKind
    = TextInputNode String
    | TextArea


{-| -}
type alias Text2 msg =
    { onChange : String -> msg
    , text : String
    , placeholder : Maybe String
    , label : Label
    }


{-| -}
textHelper : TextInput -> List (Ui.Attribute msg) -> Text2 msg -> Element msg
textHelper textInput attrs textOptions =
    {- General overview:

          - padding is used by the text area and negated in order to make the padded area clickable.

       We specifically do property redistribution using `redistribute`, which

           redistribute them to the parent, the input, or the cover.

               - fullParent -> Wrapper around label and input
               - parent -> parent of wrapper
               - wrapper -> the element that is here to take up space.
               - cover -> things like placeholders or text areas which are layered on top of input.
               - input -> actual input element

    -}
    let
        hasId attr =
            Two.keepOnly
                (\flag ->
                    BitField.fieldEqual Flag.id flag
                )
                attr

        withDefaults =
            defaultTextBoxStyle2
                ++ List.map (Two.removeIfFlag (BitField.fieldEqual Flag.id)) attrs
    in
    case textInput.type_ of
        TextArea ->
            let
                id =
                    List.filterMap
                        hasId
                        attrs

                padding =
                    List.filterMap
                        (Two.keepOnly
                            (\flag ->
                                BitField.fieldEqual Flag.padding flag
                            )
                        )
                        withDefaults

                inputElement =
                    Two.element
                        Two.NodeAsTextArea
                        Two.AsEl
                        ([ Two.class classes.inputMultiline
                         , Two.style "line-height" "inherit"
                         , Two.style "grid-column" "1 / 2"
                         , Two.style "grid-row" "1 / 2"
                         , Two.style "resize" "none"
                         , Two.style "overflow" "hidden"
                         , Two.attribute (Html.Attributes.value textOptions.text)
                         , Two.attribute (Html.Events.onInput textOptions.onChange)
                         , labelAttribute textOptions.label
                         , Two.attribute (Html.Attributes.spellcheck textInput.spellchecked)
                         , case textInput.autofill of
                            Nothing ->
                                Two.noAttr

                            Just fill ->
                                Two.attribute (Html.Attributes.attribute "autocomplete" fill)
                         , case textOptions.placeholder of
                            Nothing ->
                                Two.noAttr

                            Just placeholder ->
                                Two.attribute (Html.Attributes.placeholder placeholder)
                         ]
                            ++ id
                            ++ padding
                        )
                        []
            in
            -- In order to get growing text areas, we have a grid element and then layer two elements on top of eahc other.
            --
            Two.element Two.NodeAsDiv
                Two.AsEl
                ([ Ui.width Ui.fill
                 , Two.class classes.focusedWithin
                 , Two.class classes.inputMultilineWrapper
                 , Two.style "white-space" "pre-wrap"
                 , Two.style "display" "grid"
                 ]
                    ++ withDefaults
                    ++ [ Ui.padding 0 ]
                )
                [ Ui.el
                    (Two.style "grid-column" "1 / 2"
                        :: Two.style "grid-row" "1 / 2"
                        :: Two.style "visibility" "hidden"
                        :: padding
                    )
                  <|
                    -- We append a non-breaking space to the end of the content so that newlines don't get chomped.
                    if textOptions.text == "" then
                        -- Without this, firefox will make the text area lose focus
                        -- if the input is empty and you mash the keyboard
                        Ui.text (Maybe.withDefault "" textOptions.placeholder ++ "\u{00A0}")

                    else
                        Ui.text (textOptions.text ++ "\u{00A0}")
                , inputElement
                ]

        TextInputNode inputType ->
            Two.element
                Two.NodeAsInput
                Two.AsEl
                -- Note: Due to a weird edgecase in...Edge...
                -- `type` needs to come _before_ `value`
                -- More reading: https://github.com/mdgriffith/elm-ui/pull/94/commits/4f493a27001ccc3cf1f2baa82e092c35d3811876
                ([ Two.attribute (Html.Attributes.type_ inputType)
                 , Two.class classes.inputText
                 , Two.attribute (Html.Attributes.value textOptions.text)
                 , Two.attribute (Html.Events.onInput textOptions.onChange)
                 , labelAttribute textOptions.label
                 , Two.attribute (Html.Attributes.spellcheck textInput.spellchecked)
                 , case textInput.autofill of
                    Nothing ->
                        Two.noAttr

                    Just fill ->
                        Two.attribute (Html.Attributes.attribute "autocomplete" fill)
                 , case textOptions.placeholder of
                    Nothing ->
                        Two.noAttr

                    Just placeholder ->
                        Two.attribute (Html.Attributes.placeholder placeholder)
                 ]
                    ++ withDefaults
                )
                []


{-| -}
text :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element msg
text =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = False
        , autofill = Nothing
        }


{-| If spell checking is available, this input will be spellchecked.
-}
spellChecked :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element msg
spellChecked =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = True
        , autofill = Nothing
        }


{-| -}
search :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element msg
search =
    textHelper
        { type_ = TextInputNode "search"
        , spellchecked = False
        , autofill = Nothing
        }


{-| A password input that allows the browser to autofill.

It's `newPassword` instead of just `password` because it gives the browser a hint on what type of password input it is.

A password takes all the arguments a normal `Input.text` would, and also **show**, which will remove the password mask (e.g. `****` vs `pass1234`)

-}
newPassword :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        , show : Bool
        }
    -> Element msg
newPassword attrs pass =
    textHelper
        { type_ =
            TextInputNode <|
                if pass.show then
                    "text"

                else
                    "password"
        , spellchecked = False
        , autofill = Just "new-password"
        }
        attrs
        { onChange = pass.onChange
        , text = pass.text
        , placeholder = pass.placeholder
        , label = pass.label
        }


{-| -}
currentPassword :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        , show : Bool
        }
    -> Element msg
currentPassword attrs pass =
    textHelper
        { type_ =
            TextInputNode <|
                if pass.show then
                    "text"

                else
                    "password"
        , spellchecked = False
        , autofill = Just "current-password"
        }
        attrs
        { onChange = pass.onChange
        , text = pass.text
        , placeholder = pass.placeholder
        , label = pass.label
        }


{-| -}
username :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element msg
username =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = False
        , autofill = Just "username"
        }


{-| **Note** For some reason Firefox and Brave both require an email input to have `id=email` in order for autofill to work correctly.

If you use `labelHidden`, then elm-ui will set this behind the scenes for you.

If you use a `label` though, you'll need to do something like this:

    let
        label =
            -- the "email" string is required by firefox and brave to autofill correctly.
            Ui.Input.label "email" [] (Ui.text "Do you want Guacamole?")
    in
    Ui.column []
        [ label.element
        , Ui.Input.email []
            { onChange = EmailUpdated
            , text = model.email
            , placeholder = Nothing
            , label = label.id
            }
        ]

-}
email :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        }
    -> Element msg
email attrs =
    textHelper
        { type_ = TextInputNode "email"
        , spellchecked = False
        , autofill = Just "email"
        }
        (attrs ++ [ Ui.id "email" ])


{-| A multiline text input.

By default it will have a minimum height of one line and resize based on it's contents.

-}
multiline :
    List (Ui.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe String
        , label : Label
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs multi =
    textHelper
        { type_ = TextArea
        , spellchecked = multi.spellcheck
        , autofill = Nothing
        }
        attrs
        { onChange = multi.onChange
        , text = multi.text
        , placeholder = multi.placeholder
        , label = multi.label
        }


isHiddenLabel : Label -> Bool
isHiddenLabel lbl =
    case lbl of
        HiddenLabel _ ->
            True

        _ ->
            False


{-| -}
type Option value msg
    = Option value (OptionState -> Element msg)


{-| -}
type OptionState
    = Idle
    | Focused
    | Selected


{-| Add a choice to your chooseOne Element. This will be rendered with a default icon.
-}
option : value -> Element msg -> Option value msg
option val txt =
    Option val (defaultRadioOption txt)


{-| Customize exactly what your chooseOne option should look like in different states.
-}
optionWith : value -> (OptionState -> Element msg) -> Option value msg
optionWith val view =
    Option val view


{-| -}
chooseOne :
    (List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg)
    -> List (Ui.Attribute msg)
    ->
        { onChange : option -> msg
        , options : List (Option option msg)
        , selected : Maybe option
        , label : Label
        }
    -> Element msg
chooseOne layoutFn attrs input =
    let
        prevNext =
            case input.options of
                [] ->
                    Nothing

                (Option val _) :: _ ->
                    List.foldl track ( NotFound, val, val ) input.options
                        |> (\( found, b, a ) ->
                                case found of
                                    NotFound ->
                                        Just ( b, val )

                                    BeforeFound ->
                                        Just ( b, val )

                                    _ ->
                                        Just ( b, a )
                           )

        track opt ( found, prev, nxt ) =
            case opt of
                Option val _ ->
                    case found of
                        NotFound ->
                            if Just val == input.selected then
                                ( BeforeFound, prev, nxt )

                            else
                                ( found, val, nxt )

                        BeforeFound ->
                            ( AfterFound, prev, val )

                        AfterFound ->
                            ( found, prev, nxt )

        events =
            -- List.
            []

        --         Internal.get
        --             attrs
        --         <|
        --             \attr ->
        --                 case attr of
        --                     Internal.Width (Internal.Fill _) ->
        --                         True
        --                     Internal.Height (Internal.Fill _) ->
        --                         True
        --                     Internal.Attr _ ->
        --                         True
        --                     _ ->
        --                         False
        finalAttrs =
            [ labelAttribute input.label
            , Ui.alignLeft
            , Two.attribute (Html.Attributes.tabindex 0)
            , Two.class "focus"
            , Ui.Accessibility.announce
            , Two.attribute <|
                Html.Attributes.attribute "role" "radiogroup"
            , case prevNext of
                Nothing ->
                    Two.class ""

                Just ( prev, next ) ->
                    onKeyLookup2 <|
                        \code ->
                            if code == leftArrow then
                                Just (input.onChange prev)

                            else if code == upArrow then
                                Just (input.onChange prev)

                            else if code == rightArrow then
                                Just (input.onChange next)

                            else if code == downArrow then
                                Just (input.onChange next)

                            else if code == space then
                                case input.selected of
                                    Nothing ->
                                        Just (input.onChange prev)

                                    _ ->
                                        Nothing

                            else
                                Nothing
            ]
                ++ events
                ++ attrs
    in
    layoutFn finalAttrs
        (List.map (renderOption input) input.options)


defaultRadioOption : Element msg -> OptionState -> Element msg
defaultRadioOption optionLabel status =
    Ui.row
        [ Ui.spacing 10
        , Ui.alignLeft
        ]
        [ Ui.el
            [ Ui.width (Ui.px 14)
            , Ui.height (Ui.px 14)
            , Ui.background white
            , Ui.rounded 7
            , case status of
                Selected ->
                    Two.class "focusable"

                _ ->
                    Two.noAttr

            -- , Border.shadow <|
            --     -- case status of
            --     --     Idle ->
            --     --         { offset = ( 0, 0 )
            --     --         , blur =
            --     --             1
            --     --         , color = Color.rgb 235 235 235
            --     --         }
            --     --     Focused ->
            --     --         { offset = ( 0, 0 )
            --     --         , blur =
            --     --             0
            --     --         , color = Color.rgba 235 235 235 0
            --     --         }
            --     --     Selected ->
            --     { offset = ( 0, 0 )
            --     , blur =
            --         1
            --     , color = Color.rgba 235 235 235 0
            --     }
            , Ui.borderColor <|
                case status of
                    Idle ->
                        Ui.rgb 208 208 208

                    Focused ->
                        Ui.rgb 208 208 208

                    Selected ->
                        Ui.rgb 59 153 252
            , Ui.border <|
                case status of
                    Idle ->
                        1

                    Focused ->
                        1

                    Selected ->
                        5
            ]
            Ui.none
        , Ui.el [ Ui.width Ui.fill, Two.class "unfocusable" ] optionLabel
        ]


renderOption :
    { onChange : option -> msg
    , options : List (Option option msg)
    , selected : Maybe option
    , label : Label
    }
    -> Option option msg
    -> Element msg
renderOption input (Option val view) =
    let
        status =
            if Just val == input.selected then
                Selected

            else
                Idle
    in
    Ui.el
        [ Ui.pointer
        , Ui.Events.onClick (input.onChange val)
        , Two.attribute <|
            Html.Attributes.attribute "aria-checked"
                (case status of
                    Selected ->
                        "true"

                    _ ->
                        "false"
                )
        , Two.attribute <|
            Html.Attributes.attribute "role" "radio"
        ]
        (view status)


type Found
    = NotFound
    | BeforeFound
    | AfterFound



{- Event Handlers -}


enter : String
enter =
    "Enter"


tab : String
tab =
    "Tab"


delete : String
delete =
    "Delete"


backspace : String
backspace =
    "Backspace"


upArrow : String
upArrow =
    "ArrowUp"


leftArrow : String
leftArrow =
    "ArrowLeft"


rightArrow : String
rightArrow =
    "ArrowRight"


downArrow : String
downArrow =
    "ArrowDown"


space : String
space =
    " "


{-| -}
onKeyLookup2 : (String -> Maybe msg) -> Ui.Attribute msg
onKeyLookup2 lookup =
    let
        decode code =
            case lookup code of
                Nothing ->
                    Json.fail "No key matched"

                Just msg ->
                    Json.succeed msg

        isKey =
            Json.field "key" Json.string
                |> Json.andThen decode
    in
    Two.attribute <| Html.Events.on "keyup" isKey



{- Style Defaults -}


defaultTextBoxStyle2 : List (Ui.Attribute msg)
defaultTextBoxStyle2 =
    [ Ui.padding 12
    , Ui.rounded 3
    , Ui.borderColor darkGrey2
    , Ui.border 1
    , Ui.background white
    , Ui.spacing 5
    , Ui.width Ui.fill

    -- TODO: SHOULD BE HEIGHT SHRINK
    ]


{-| The blue default checked box icon.

You'll likely want to make your own checkbox at some point that fits your design.

-}
defaultCheckbox : Bool -> Element msg
defaultCheckbox checked =
    Ui.el
        [ Ui.width (Ui.px 14)
        , Ui.height (Ui.px 14)
        , Ui.Font.color white
        , Ui.Font.size 9
        , Ui.Font.center
        , Ui.centerY
        , Ui.rounded 3
        , Ui.borderColor <|
            if checked then
                Ui.rgb 59 153 252

            else
                Ui.rgb 211 211 211
        , Ui.border <|
            if checked then
                0

            else
                1
        , if checked then
            Ui.noAttr

          else
            Ui.Shadow.shadows
                [ { x = 0
                  , y = 0
                  , blur = 1
                  , size = 1
                  , color =
                        Ui.rgb 238 238 238
                  }
                ]
        , Ui.background <|
            if checked then
                blue

            else
                white
        ]
        (if checked then
            Ui.el
                [ Ui.borderWith
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                , Ui.borderColor white
                , Ui.height (Ui.px 6)
                , Ui.width (Ui.px 9)
                , Ui.rotate (Ui.turns (1 - 0.125))
                , Ui.centerX
                , Ui.centerY
                , Ui.move (Ui.up 1)
                ]
                Ui.none

         else
            Ui.none
        )
