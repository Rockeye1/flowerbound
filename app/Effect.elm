module Effect exposing (Effect(..), batch, fromCmd, map, none, perform, rollCheck)

{-|

@docs Effect, batch, fromCmd, map, none, perform, rollCheck

-}

import Browser.Navigation
import File exposing (File)
import File.Select
import Parser
import Persona.Codec
import Random
import Task
import Types exposing (Persona)


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | SetRouteToPersona Persona
    | PickMarkdown (File -> msg)
    | ReadPersonaFromMarkdown File (Result String Persona -> msg)
    | Roll Int (Int -> msg)
    | RollStimulation (List Int) (List ( Int, Int ) -> msg)


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


rollCheck : Int -> (Int -> msg) -> Effect msg
rollCheck bonus toMsg =
    Roll 10 (\res -> toMsg (res + bonus))


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
                (\file -> file |> toMsg |> fn)

        ReadPersonaFromMarkdown file toMsg ->
            ReadPersonaFromMarkdown file
                (\result -> result |> toMsg |> fn)

        Roll die toMsg ->
            Roll die
                (\result -> result |> toMsg |> fn)

        RollStimulation dice toMsg ->
            RollStimulation dice
                (\result -> result |> toMsg |> fn)


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

        Roll die toMsg ->
            Random.int 1 die
                |> Random.generate
                    (\result -> fromPageMsg (toMsg result))

        RollStimulation dice toMsg ->
            List.foldr
                (\die acc ->
                    Random.map2
                        (::)
                        (Random.pair (Random.int 1 die) (Random.int 1 die))
                        acc
                )
                (Random.constant [])
                dice
                |> Random.generate
                    (\result -> fromPageMsg (toMsg result))


parserErrorToString : String -> List Parser.DeadEnd -> String
parserErrorToString _ _ =
    "TODO: parserErrorToString"
