module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

{-|

@docs Effect, batch, fromCmd, map, none, perform

-}

import Browser.Navigation
import File exposing (File)
import File.Select
import Parser
import Persona.Codec
import Persona.Types exposing (Persona)
import Task


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | SetRouteToPersona Persona
    | PickMarkdown (File -> msg)
    | ReadPersonaFromMarkdown File (Result String Persona -> msg)


{-| -}
none : Effect msg
none =
    None


{-| -}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| -}
fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


{-| -}
map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        SetRouteToPersona persona ->
            SetRouteToPersona persona

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Batch list ->
            Batch (List.map (map fn) list)

        PickMarkdown toMsg ->
            PickMarkdown
                (\file ->
                    file
                        |> toMsg
                        |> fn
                )

        ReadPersonaFromMarkdown file toMsg ->
            ReadPersonaFromMarkdown file
                (\result ->
                    result
                        |> toMsg
                        |> fn
                )


{-| -}
perform :
    { config
        | fromPageMsg : pageMsg -> msg
        , key : Browser.Navigation.Key
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fromPageMsg, key } as helpers) effect =
    case effect of
        None ->
            Cmd.none

        SetRouteToPersona persona ->
            persona
                |> Persona.Codec.toUrl
                |> Browser.Navigation.replaceUrl key

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Batch list ->
            Cmd.batch (List.map (perform helpers) list)

        PickMarkdown toMsg ->
            File.Select.file
                [ ".md"
                , "text/plain"
                ]
                (\file -> fromPageMsg (toMsg file))

        ReadPersonaFromMarkdown file toMsg ->
            File.toString file
                |> Task.perform
                    (\markdown ->
                        Parser.run Persona.Codec.personaParser markdown
                            |> Result.mapError (parserErrorToString markdown)
                            |> toMsg
                            |> fromPageMsg
                    )


parserErrorToString : String -> List Parser.DeadEnd -> String
parserErrorToString _ _ =
    "TODO: parserErrorToString"
