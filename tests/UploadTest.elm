module UploadTest exposing (roundtrip)

import Effect
import Expect
import Parser
import Persona
import Persona.Codec
import Persona.Types exposing (Persona)
import Result.Extra
import Route
import Route.Persona.Name_.Data__
import Test exposing (Test, test)
import Url


roundtrip : Test
roundtrip =
    test "Roundtrips" <|
        \_ ->
            case Parser.run Persona.Codec.personaParser ishaza of
                Err e ->
                    Expect.fail (Debug.toString e)

                Ok loaded ->
                    let
                        ( model, effect, shared ) =
                            Route.Persona.Name_.Data__.setPersona loaded
                    in
                    if model /= loaded then
                        model |> Expect.equal loaded

                    else if shared /= Nothing then
                        shared |> Expect.equal Nothing

                    else
                        (case effect of
                            Effect.SetRoute route hash ->
                                Ok { route = route, hash = hash }

                            _ ->
                                Err "Unexpected effect"
                        )
                            |> Result.andThen
                                (\{ route, hash } ->
                                    case Route.urlToRoute { path = Route.toString route } of
                                        Just (Route.Persona__Name___Data__ { name, data }) ->
                                            case data of
                                                Nothing ->
                                                    Err "No data extracted"

                                                Just d ->
                                                    Ok { hash = hash, name = name, data = d }

                                        parsed ->
                                            Err ("Unexpected route: " ++ Debug.toString parsed)
                                )
                            |> Result.andThen
                                (\{ hash, name, data } ->
                                    case Url.percentDecode name of
                                        Nothing ->
                                            Err "Could not decode the name"

                                        Just decodedName ->
                                            Ok
                                                { hash = hash
                                                , data = data
                                                , decodedName = decodedName
                                                }
                                )
                            |> Result.andThen
                                (\{ hash, data, decodedName } ->
                                    case Route.Persona.Name_.Data__.partialPersonaFromSlug data of
                                        Err e ->
                                            Err
                                                ("Could not get persona from slug ("
                                                    ++ data
                                                    ++ "): "
                                                    ++ e
                                                )

                                        Ok partial ->
                                            Ok
                                                { hash = hash
                                                , decodedName = decodedName
                                                , partial = partial
                                                }
                                )
                            |> Result.andThen
                                (\{ hash, decodedName, partial } ->
                                    case Route.Persona.Name_.Data__.fragmentToGendertropeRecord hash of
                                        Nothing ->
                                            Err "Could not get gendertrope record from hash"

                                        Just gendertropeRecord ->
                                            let
                                                recovered : Persona
                                                recovered =
                                                    Persona.fromPartial decodedName (Just partial) (Just gendertropeRecord)
                                            in
                                            recovered
                                                |> Expect.equal loaded
                                                |> Ok
                                )
                            |> Result.mapError Expect.fail
                            |> Result.Extra.merge


ishaza : String
ishaza =
    """# Ishaza Thorn

## Ability Scores
- Fitness: 2
- Grace: 2
- Ardor: 3
- Sanity: 3
- Prowess: 4
- Moxie: 4

## Progression Tally
- Euphoria Points: 0
- Ichor Points: 0
- Numinous Points: 0

## Unlocked features
- Level 1

## Gendertrope: The Weaver
She is an artist of implication and a crafter of pleasures. Her purpose and joy is to serve well, and she bends all her faculties to this task. But she is a prideful creature, and demands that those who would own her first defeat her at her own games.

### Organs
- Mouth: Silver-Painted Lips
- Hands: Tender Graceful Fingers
- Breasts: Temptingly Soft Bosom
- Hips: Firm Voluptuous Ass
- Yonic: Slick Sumptuous Folds
- Legs: Slender Stately Legs

### Level 1 Feature: Perfected Protocol
When a partner attempts to firmly or violently pair to one of your Organs, they must first pass a Sanity check opposed by your Prowess. If they fail, they expend no stamina but cannot pair to that organ this turn.

### Level 2 Feature: Graceful Weaver
You permanently gain access to the following moves:
 
> **Waking Touch** (Tease) [Grips/Ensheathes] | CT: **0**|
>
> If this move Understimulates the target, apply the **Teased** effect to the target of this move.
>
> If the Organ using this move is your _Silver-Painted Lips_ or your _Tender Graceful Fingers_, add +1 to this move's attempted Stimulation, and also gain 1 Craving.
 
> **Swirling Suction** (Thrust) [Ensheathes] | CT: **10** |
>
> If the target of this move has the **Teased** effect, the organ targeted by this move is treated as having additional Erogeny equal to your Ardor. Add your Ardor to this move's attempted Stimulation.
 
> **Empathic Touch** (Tease) [Squishes/Grips/Ensheathes/Penetrates] | CT: **20** |
>
> You may demand to know how far the target of this move is from their Satiation Cap.
>
> You may truthfully share this information from your own Persona Sheet.
>
> Roll a Moxie check, and remove that much Sensitivity from both you and the target of this move.

### Level 3 Feature: Surpassing Skill
> If your partner's genitals are Paired with your _Tender Graceful Fingers_ or your _Silver-Painted Lips_ and they **Have An Orgasm,** they may roll a Sanity Check. If the result of the Check is not greater than your Prowess, or they choose not to roll, they become mesmerized by your surpassing technique and gain the **Fixation** effect.

> **Fixation** _Passive_
>
> You have disadvantage on all actions that do not target the Organ that inflicted this effect. You cannot volitionally Unpair from the Organ that inflicted this effect.
>
> At the beginning of your turn, you may roll a Sanity Check. If the result of the Check is greater than your Craving value, you may remove this effect.

### Level 4 Feature: -


### Level 5 Feature: -
"""
