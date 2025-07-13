module Types exposing (Action(..), Appendage, Feature, Gendertrope(..), GendertropeRecord, Move, Organ, OrganOrAppendage, OrganType(..), PartialGendertrope(..), PartialPersona, Persona, StimulationType(..), actionToCan, actionToCanIcon, actionToInitial, actionToIs, actionToIsIcon, actionToString, standardGendertropes, stimulationTypeToString)

import Dict exposing (Dict)
import Icons
import Phosphor exposing (IconVariant)


type alias PartialPersona =
    { hue : Maybe Float

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
    , gendertrope : PartialGendertrope
    }


type alias Persona =
    { name : String
    , hue : Maybe Float

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
    | SeedStalker
    | Flower
    | Housepet
    | Vixen
    | Buck
    | Fiend
    | Doll
    | JunglePrince
    | Firecracker
    | DemonQueen
    | Eldritch
    | Custom GendertropeRecord


type PartialGendertrope
    = PartialButterfly
    | PartialSeedStalker
    | PartialFlower
    | PartialHousepet
    | PartialVixen
    | PartialBuck
    | PartialFiend
    | PartialDoll
    | PartialJunglePrince
    | PartialFirecracker
    | PartialDemonQueen
    | PartialEldritch
    | PartialCustom String


standardGendertropes : List Gendertrope
standardGendertropes =
    [ Butterfly
    , Flower
    , Vixen
    , Buck
    , Fiend
    , Doll
    , SeedStalker
    , Housepet
    , JunglePrince
    , Firecracker
    , DemonQueen
    , Eldritch
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
    , type_ : OrganType
    , contour : Int
    , erogeny : Int
    , appendages : List Appendage
    , canSquish : Bool
    , canGrip : Bool
    , canPenetrate : Bool
    , canEnsheathe : Bool
    , isSquishable : Bool
    , isGrippable : Bool
    , isPenetrable : Bool
    , isEnsheatheable : Bool
    }


type alias Appendage =
    { name : String
    , canSquish : Bool
    , canGrip : Bool
    , canPenetrate : Bool
    , canEnsheathe : Bool
    , isSquishable : Bool
    , isGrippable : Bool
    , isPenetrable : Bool
    , isEnsheatheable : Bool
    }


type alias OrganOrAppendage a =
    { a
        | name : String
        , canSquish : Bool
        , canGrip : Bool
        , canPenetrate : Bool
        , canEnsheathe : Bool
        , isSquishable : Bool
        , isGrippable : Bool
        , isPenetrable : Bool
        , isEnsheatheable : Bool
    }


type OrganType
    = Mouth
    | Hands
    | Breasts
    | Hips
    | Yonic
    | Phallic
    | Legs
    | Prehensile
    | Other


type alias Move =
    { name : String
    , stimulationType : StimulationType
    , actionCompatibility : List Action
    , cravingThreshold : Int
    , description : String
    }


type StimulationType
    = Tease
    | Grind
    | Thrust


type Action
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


actionToString : Action -> String
actionToString attribute =
    case attribute of
        Squishes ->
            "Squishes"

        Grips ->
            "Grips"

        Penetrates ->
            "Penetrates"

        Ensheathes ->
            "Ensheathes"


actionToCan : Action -> String
actionToCan attribute =
    case attribute of
        Squishes ->
            "Can Squish"

        Grips ->
            "Can Grip"

        Penetrates ->
            "Can Penetrate"

        Ensheathes ->
            "Can Ensheathe"


actionToIs : Action -> String
actionToIs attribute =
    case attribute of
        Squishes ->
            "Is Squishable"

        Grips ->
            "Is Grippable"

        Penetrates ->
            "Is Penetrable"

        Ensheathes ->
            "Is Ensheatheable"


actionToCanIcon : Action -> IconVariant
actionToCanIcon attribute =
    case attribute of
        Squishes ->
            Icons.squish

        Grips ->
            Icons.grip

        Penetrates ->
            Icons.penetrate

        Ensheathes ->
            Icons.ensheathe


actionToIsIcon : Action -> IconVariant
actionToIsIcon attribute =
    case attribute of
        Squishes ->
            Icons.squishable

        Grips ->
            Icons.grippable

        Penetrates ->
            Icons.penetrable

        Ensheathes ->
            Icons.ensheatheable


actionToInitial : Action -> String
actionToInitial attribute =
    case attribute of
        Squishes ->
            "S"

        Grips ->
            "G"

        Penetrates ->
            "P"

        Ensheathes ->
            "E"
