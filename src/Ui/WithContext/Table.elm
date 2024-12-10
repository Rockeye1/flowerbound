module Ui.WithContext.Table exposing (Cell, Column, Config, cell, columnWithState, columns, view)

import Ui.Table
import Ui.WithContext as Ui exposing (Attribute, Element)


{-| -}
type Cell context msg
    = Cell (context -> Ui.Table.Cell msg)


{-| -}
type Config context globalState rowState data msg
    = Config (context -> Ui.Table.Config globalState rowState data msg)


type Column context globalState rowState data msg
    = Column (context -> Ui.Table.Column globalState rowState data msg)


view :
    List (Attribute context msg)
    -> Config context () () data msg
    -> List data
    -> Element context msg
view attrs (Config config) data =
    Ui.fromContext
        (\context ->
            Ui.withAttrs
                (\oattrs () ->
                    Ui.Table.view oattrs
                        (config context)
                        data
                )
                attrs
                ()
        )


columns :
    List (Column context globalState rowState data msg)
    -> Config context globalState rowState data msg
columns cols =
    Config
        (\context ->
            cols
                |> List.map (\(Column col) -> col context)
                |> Ui.Table.columns
        )


columnWithState :
    { header : globalState -> Cell context msg
    , view : Int -> Maybe rowState -> data -> Cell context msg
    }
    -> Column context globalState rowState data msg
columnWithState value =
    Column
        (\context ->
            Ui.Table.columnWithState
                { header =
                    \globalState ->
                        let
                            (Cell c) =
                                value.header globalState
                        in
                        c context
                , view =
                    \index state data ->
                        let
                            (Cell c) =
                                value.view index state data
                        in
                        c context
                }
        )


cell : List (Attribute context msg) -> Element context msg -> Cell context msg
cell attrs child =
    Cell
        (\context ->
            Ui.Table.cell
                (List.map (Ui.withContextAttribute context) attrs)
                (Ui.withContext context child)
        )
