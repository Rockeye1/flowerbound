module Ui.WithContext.Prose exposing (paragraph)

import Ui.Prose
import Ui.WithContext as Ui exposing (Attribute, Element)


paragraph :
    List (Attribute context msg)
    -> List (Element context msg)
    -> Element context msg
paragraph attrs children =
    Ui.withChildren Ui.Prose.paragraph attrs children
