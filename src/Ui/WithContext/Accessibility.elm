module Ui.WithContext.Accessibility exposing (h1, h2, h3, h4, h5, h6)

import Ui.Accessibility
import Ui.WithContext as Ui exposing (Attribute)


h1 : Attribute context msg
h1 =
    Ui.liftAttribute Ui.Accessibility.h1


h2 : Attribute context msg
h2 =
    Ui.liftAttribute Ui.Accessibility.h2


h3 : Attribute context msg
h3 =
    Ui.liftAttribute Ui.Accessibility.h3


h4 : Attribute context msg
h4 =
    Ui.liftAttribute Ui.Accessibility.h4


h5 : Attribute context msg
h5 =
    Ui.liftAttribute Ui.Accessibility.h5


h6 : Attribute context msg
h6 =
    Ui.liftAttribute Ui.Accessibility.h6
