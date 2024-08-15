module Types exposing (Flags, Gendertrope(..), Model, Msg(..), Persona)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    { key : Key
    , personas : List { flipped : Bool, persona : Persona }
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
    , stamina : Int
    , satiation : Int
    , craving : Int
    , arousal : Int
    , sensitivity : Int

    --
    , euphoriaPoints : Int
    , ichorPoints : Int
    , numinousPoints : Int
    , gendertrope : Gendertrope
    }


type Gendertrope
    = TheButterfly


type Msg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChangePersona Int Persona
    | Flip Int
    | NoOp
