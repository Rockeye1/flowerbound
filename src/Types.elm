module Types exposing (Attribute(..), Feature, Gendertrope(..), GendertropeRecord, Move, Organ, PartialGendertrope(..), PartialPersona, Persona, StimulationType(..), attributeToIcon, attributeToString, standardGendertropes, stimulationTypeToString)

import Dict exposing (Dict)
import Icons
import Phosphor exposing (IconVariant)


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
    , gendertrope : PartialGendertrope
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

    -- These are kept as the total ever gained
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


type PartialGendertrope
    = PartialButterfly
    | PartialFlower
    | PartialVixen
    | PartialBuck
    | PartialFiend
    | PartialDoll
    | PartialCustom String


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
    , organs : List Organ
    , features : Dict Int Feature
    , icon :
        Maybe
            { opaque : List String
            , semitransparent : List String
            }
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


type alias Move =
    { name : String
    , stimulationType : StimulationType
    , attributeCompatibility : List Attribute
    , cravingThreshold : Int
    , description : String
    }


type StimulationType
    = Tease
    | Grind
    | Thrust


type Attribute
    = Squishes
    | Grips
    | Penetrates
    | Ensheathes


stimulationTypeToString : StimulationType -> String
stimulationTypeToString type_ =
    case type_ of
        Tease ->
            "Tease"

        Grind ->
            "Grind"

        Thrust ->
            "Thrust"


attributeToString : Attribute -> String
attributeToString attribute =
    case attribute of
        Squishes ->
            "Squishes"

        Grips ->
            "Grips"

        Penetrates ->
            "Penetrates"

        Ensheathes ->
            "Ensheathes"


attributeToIcon : Attribute -> IconVariant
attributeToIcon attribute =
    case attribute of
        Squishes ->
            Icons.squish

        Grips ->
            Icons.grip

        Penetrates ->
            Icons.penetrate

        Ensheathes ->
            Icons.ensheathe
