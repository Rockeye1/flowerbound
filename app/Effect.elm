module Effect exposing (Effect(..), batch, fromCmd, map, none, perform, rollCheck)

{-|

@docs Effect, batch, fromCmd, map, none, perform, rollCheck

-}

import Browser.Navigation
import File exposing (File)
import File.Select
import Json.Encode
import List.Extra
import Parser
import Persona.Codec
import Random
import Set
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
parserErrorToString input err =
    err
        |> List.Extra.gatherEqualsBy (\{ row, col } -> ( row, col ))
        |> List.map (errorToString input)
        |> String.join "\n\n"


errorToString : String -> ( Parser.DeadEnd, List Parser.DeadEnd ) -> String
errorToString source ( error, errors ) =
    let
        -- How many lines of context to show
        context : Int
        context =
            4

        lines : List String
        lines =
            String.split "\n" source
                |> List.drop (error.row - context)
                |> List.take (context * 2)

        errorString : String
        errorString =
            [ String.repeat (error.col - 1) " "
            , {- Console.red -} "^ "
            , " at row "
            , String.fromInt error.row
            , ", col "
            , String.fromInt error.col
            , " "
            , (error :: errors)
                |> List.map (\{ problem } -> problemToString problem)
                |> Set.fromList
                |> Set.toList
                |> String.join ", "

            -- |> Console.yellow
            ]
                |> String.concat

        before : Int
        before =
            min error.row context
    in
    List.take before lines
        ++ errorString
        :: List.drop before lines
        |> String.join "\n"


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.BadRepeat ->
            "Bad repeat"

        Parser.Expecting something ->
            "Expecting " ++ something

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol value ->
            "Expecting symbol " ++ escape value

        Parser.ExpectingKeyword value ->
            "Expecting keyword " ++ escape value

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem p ->
            p


escape : String -> String
escape input =
    Json.Encode.encode 0 (Json.Encode.string input)
