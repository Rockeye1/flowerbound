module Persona.View exposing (organs)

import Element exposing (Element, centerX, el, fill, height, padding, shrink, text, width)
import Element.Background as Background
import Element.Font as Font
import Icons
import Persona.Types exposing (Organ)
import Theme


organs : List Organ -> Element msg
organs input =
    let
        wrap : Int -> Element msg -> Element msg
        wrap index child =
            el
                [ width fill
                , height fill
                , padding (Theme.rhythm // 2)
                , Background.color
                    (if modBy 2 index == 0 then
                        Theme.lightGray

                     else
                        Theme.white
                    )
                ]
                child

        intColumn :
            String
            -> (Organ -> Int)
            -> Element.IndexedColumn Organ msg
        intColumn label prop =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    wrap index
                        -- (el
                        --     [ Font.color Theme.purple
                        --     , Font.size 24
                        --     , centerX
                        --     ]
                        --     (text (intToDots (prop organ)))
                        -- )
                        (el
                            [ centerX
                            ]
                            (text (String.fromInt (prop organ)))
                        )
            }

        boolColumn :
            String
            -> (Organ -> Bool)
            -> Element msg
            -> Element.IndexedColumn Organ msg
        boolColumn label prop img =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    if prop organ then
                        wrap index (el [ centerX, Font.color Theme.purple ] img)

                    else
                        wrap index Element.none
            }

        spacer : Element.IndexedColumn organ msg
        spacer =
            { width = fill
            , header = Element.none
            , view = \_ _ -> Element.none
            }
    in
    Element.indexedTable [ width fill ]
        { data = input
        , columns =
            [ spacer
            , { width = shrink
              , header = Element.none
              , view = \index { name } -> wrap index (text name)
              }
            , intColumn "Cont" .contour
            , intColumn "Erog" .erogeny
            , boolColumn "CS" .canSquish Icons.squish
            , boolColumn "CG" .canGrip Icons.grip
            , boolColumn "CP" .canPenetrate Icons.penetrate
            , boolColumn "CE" .canEnsheathe Icons.ensheathe
            , boolColumn "IS" .isSquishable Icons.squishable
            , boolColumn "IG" .isGrippable Icons.grippable
            , boolColumn "IP" .isPenetrable Icons.penetrable
            , boolColumn "IE" .isEnsheatheable Icons.ensheatheable
            , spacer
            ]
        }
