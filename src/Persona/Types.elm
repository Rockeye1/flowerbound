module Persona.Types exposing (Feature, Gendertrope(..), GendertropeRecord, Organ, PartialPersona, Persona, standardGendertropes)

import Dict exposing (Dict)


type alias PartialPersona =
    { fitness : Int
    , grace : Int
    , ardor : Int
    , sanity : Int
    , prowess : Int
    , moxie : Int

    --
    , euphoriaPoints : Int
    , ichorPoints : Int
    , numinousPoints : Int

    --
    , features : List Int
    , gendertrope : Gendertrope
    }


type alias Persona =
    { name : String

    --
    , fitness : Int
    , grace : Int
    , ardor : Int
    , sanity : Int
    , prowess : Int
    , moxie : Int

    --
    , euphoriaPoints : Int
    , ichorPoints : Int
    , numinousPoints : Int

    --
    , features : List Int
    , gendertrope : Gendertrope
    }


type Gendertrope
    = Butterfly
    | Flower
    | Vixen
    | Buck
    | Fiend
    | Doll
    | Custom GendertropeRecord


standardGendertropes : List Gendertrope
standardGendertropes =
    [ Butterfly
    , Flower
    , Vixen
    , Buck
    , Fiend
    , Doll
    ]


type alias GendertropeRecord =
    { name : String
    , description : String
    , features : Dict Int Feature
    , organs : List Organ
    }


type alias Feature =
    { name : String
    , description : String
    }


type alias Organ =
    { name : String
    , contour : Int
    , erogeny : Int
    , canSquish : Bool
    , canGrip : Bool
    , canPenetrate : Bool
    , canEnsheathe : Bool
    , isSquishable : Bool
    , isGrippable : Bool
    , isPenetrable : Bool
    , isEnsheatheable : Bool
    }
