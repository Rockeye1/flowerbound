module View exposing (View, map)

{-|

@docs View, map

-}

import Theme exposing (Element)
import Ui.WithContext as Ui


{-| -}
type alias View msg =
    { title : String
    , body : Element msg
    }


{-| -}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = Ui.map fn doc.body
    }
