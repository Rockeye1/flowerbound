module GendertropeNameRoundtripTest exposing (roundtrips)

import Expect
import Persona.Data
import Test exposing (Test)
import Types


roundtrips : Test
roundtrips =
    Types.standardGendertropes
        |> List.map
            (\gendertrope ->
                let
                    { name } =
                        Persona.Data.gendertropeToRecord gendertrope
                in
                Test.test (name ++ " roundtrips") <|
                    \_ ->
                        name
                            |> Persona.Data.gendertropeFromName
                            |> Expect.equal (Just gendertrope)
            )
        |> Test.describe "Check that all standard gendertropes names roundtrip"
