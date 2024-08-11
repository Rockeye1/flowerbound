module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , personas : List Persona
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
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChangePersona Int Persona
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
