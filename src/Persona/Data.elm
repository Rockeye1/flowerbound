module Persona.Data exposing (breasts, emptyAppendage, gendertropeFromName, gendertropeIcon, gendertropeIconElement, gendertropeToRecord, hands, hips, legs, mouth, organTypeToIcon, organTypeToString, organTypes, other, phallic, prehensile, yonic)

import Dict
import Icons
import Phosphor exposing (IconVariant)
import Svg
import Svg.Attributes
import Types exposing (Appendage, Feature, Gendertrope(..), GendertropeRecord, Organ, OrganType(..))
import Ui.WithContext exposing (Element)


gendertropeIconElement : Gendertrope -> Element context msg
gendertropeIconElement gendertrope =
    gendertropeIcon gendertrope
        |> Icons.toElement


gendertropeIcon : Gendertrope -> Phosphor.IconVariant
gendertropeIcon gendertrope =
    case gendertrope of
        Butterfly ->
            Icons.butterfly

        SeedStalker ->
            Icons.seedstalker

        Flower ->
            Icons.flower

        Housepet ->
            Icons.housepet

        Vixen ->
            Icons.vixen

        Buck ->
            Icons.buck

        Fiend ->
            Icons.fiend

        Doll ->
            Icons.doll

        JunglePrince ->
            Icons.junglePrince

        Firecracker ->
            Icons.firecracker

        DemonQueen ->
            Icons.demonQueen

        Eldritch ->
            Icons.eldritch

        Custom { name, icon } ->
            case icon of
                Just { semitransparent, opaque } ->
                    (List.map
                        (\path ->
                            Svg.path
                                [ Svg.Attributes.d path
                                , Svg.Attributes.opacity "0.2"
                                ]
                                []
                        )
                        semitransparent
                        ++ List.map
                            (\path ->
                                Svg.path
                                    [ Svg.Attributes.d path ]
                                    []
                            )
                            opaque
                    )
                        |> Phosphor.customIcon

                Nothing ->
                    case gendertropeFromName name of
                        Just g ->
                            gendertropeIcon g

                        Nothing ->
                            Icons.custom


gendertropeToRecord : Gendertrope -> GendertropeRecord
gendertropeToRecord gendertrope =
    case gendertrope of
        Butterfly ->
            butterfly

        SeedStalker ->
            seedstalker

        Flower ->
            flower

        Housepet ->
            housepet

        Vixen ->
            vixen

        Buck ->
            buck

        Fiend ->
            fiend

        Doll ->
            doll

        JunglePrince ->
            junglePrince

        Firecracker ->
            firecracker

        DemonQueen ->
            demonQueen

        Eldritch ->
            eldritch

        Custom record ->
            record


emptyOrgan : Organ
emptyOrgan =
    { name = ""
    , type_ = Other
    , contour = 0
    , erogeny = 0
    , appendages = []
    , canSquish = False
    , canGrip = False
    , canPenetrate = False
    , canEnsheathe = False
    , isSquishable = False
    , isGrippable = False
    , isPenetrable = False
    , isEnsheatheable = False
    }


emptyAppendage : Appendage
emptyAppendage =
    { name = ""
    , canSquish = False
    , canGrip = False
    , canPenetrate = False
    , canEnsheathe = False
    , isSquishable = False
    , isGrippable = False
    , isPenetrable = False
    , isEnsheatheable = False
    }


mouth : String -> Organ
mouth name =
    { emptyOrgan
        | name = name
        , type_ = Mouth
        , contour = 1
        , erogeny = 2
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isPenetrable = True
    }


hands : String -> Organ
hands name =
    { emptyOrgan
        | name = name
        , type_ = Hands
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }
        |> bilateral


bilateral : Organ -> Organ
bilateral organ =
    { organ
        | appendages =
            [ { name = "Left"
              , canSquish = organ.canSquish
              , canGrip = organ.canGrip
              , canPenetrate = organ.canPenetrate
              , canEnsheathe = organ.canEnsheathe
              , isSquishable = organ.isSquishable
              , isGrippable = organ.isGrippable
              , isPenetrable = organ.isPenetrable
              , isEnsheatheable = organ.isEnsheatheable
              }
            , { name = "Right"
              , canSquish = organ.canSquish
              , canGrip = organ.canGrip
              , canPenetrate = organ.canPenetrate
              , canEnsheathe = organ.canEnsheathe
              , isSquishable = organ.isSquishable
              , isGrippable = organ.isGrippable
              , isPenetrable = organ.isPenetrable
              , isEnsheatheable = organ.isEnsheatheable
              }
            ]
    }


breasts : String -> String -> Organ
breasts name cleavage =
    { emptyOrgan
        | name = name
        , type_ = Breasts
        , contour = 5
        , erogeny = 4
        , canSquish = True
        , isSquishable = True
        , isGrippable = True
    }
        |> bilateral
        |> withAppendage
            { emptyAppendage
                | name = "Cleavage " ++ cleavage
                , canSquish = True
                , canEnsheathe = True
                , isPenetrable = True
            }


chest : String -> Organ
chest name =
    { emptyOrgan
        | name = name
        , type_ = Breasts
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , isSquishable = True
    }
        |> bilateral


withAppendage : Appendage -> Organ -> Organ
withAppendage appendage organ =
    { organ | appendages = organ.appendages ++ [ appendage ] }


hips : String -> Organ
hips name =
    { emptyOrgan
        | name = name
        , type_ = Hips
        , contour = 1
        , erogeny = 4
        , appendages =
            [ { emptyAppendage
                | name = "Orifice"
                , isPenetrable = True
                , canEnsheathe = True
              }
            , { emptyAppendage
                | name = "Flanks"
                , isSquishable = True
                , isGrippable = True
                , canSquish = True
              }
            ]
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isGrippable = True
        , isPenetrable = True
    }


legs : String -> String -> Organ
legs name thighgap =
    { emptyOrgan
        | name = name
        , type_ = Legs
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , isGrippable = True
    }
        |> bilateral
        |> withAppendage
            { emptyAppendage
                | name = thighgap ++ " Thighgap"
                , canEnsheathe = True
                , canGrip = True
                , isPenetrable = True
            }
        |> withAppendage
            { emptyAppendage
                | name = "Linked Feet"
                , canSquish = True
                , canGrip = True
                , isGrippable = True
            }


phallic : String -> Organ
phallic name =
    { emptyOrgan
        | name = name
        , type_ = Phallic
        , contour = 2
        , erogeny = 7
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


yonic : String -> Organ
yonic name =
    { emptyOrgan
        | name = name
        , type_ = Yonic
        , contour = 3
        , erogeny = 6
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isPenetrable = True
    }


prehensile : String -> Organ
prehensile name =
    { emptyOrgan
        | name = name
        , type_ = Prehensile
        , contour = 4
        , erogeny = 2
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


other : String -> Organ
other name =
    { emptyOrgan | name = name }


butterfly : GendertropeRecord
butterfly =
    { name = "The Butterfly"
    , description =
        "She is a creature of monstrous beauty and merciful power. Her amorous desires violate boundaries and overwhelm all resistance, rapacious and indomitable. But she is a nest-builder, a nurturer, one who cares for and cultivates that which her appetites have claimed as hers."
    , features =
        [ ( 1, prehensileProficiency )
        , ( 2, dominantExemplar )
        , ( 3, ambrosia )
        , ( 4, gardenKeeper )
        , ( 5, fairyFlight )
        ]
            |> Dict.fromList
    , organs =
        [ { emptyOrgan
            | name = "Shiny Thirsting Lips"
            , type_ = Mouth
            , contour = 1
            , erogeny = 2
          }
            |> withAppendage
                { emptyAppendage
                    | name = "Shiny Kissable Lips"
                    , canSquish = True
                    , canEnsheathe = True
                    , isPenetrable = True
                    , isSquishable = True
                }
        , prehensile "Sinuous Tentacle Tongue"
        , hands "Slender Elegant Hands"
        , breasts "Perky Marshmallow Tits" "Cleft"
        , hips "Tight Supple Ass"
        , phallic "Veiny Futa Phallus"
        , legs "Long Shapely Legs" "Tight"
        ]
    , icon = Nothing
    }


prehensileProficiency : Feature
prehensileProficiency =
    { name = "Prehensile Proficiency"
    , description = """When using an Organ in the "**Prehensile**" Category to make a Prowess Roll, you may make the roll with __advantage__."""
    }


dominantExemplar : Feature
dominantExemplar =
    { name = "Dominant Exemplar"
    , description = """You now have a pool of **Dominance Points** with capacity: **3**. Dominance Points do not persist between Encounters and begin at **0**.

You also permanently gain access to these three **Moves**:

> **Assertive Grope** (Tease) [Grips] | CT **5** |
>
> If, and only if, the Stimulation dealt by this Move is __ideal__, causing **0** Understimulation _and_ **0** Overstimulation, apply the **Subspace** effect to the target of this Move.

> **Wrecking Rut** (Thrust) [Penetrates] | CT **20** |
>
> If this Move deals Stimulation equal to or greater than the target's Sanity score, _and_ if the target of this Move has the **Subspace** effect, gain **1 Dominance Point**.

> **Plundering Plunge** (Thrust) [Penetrates] | CT **0** |
>
> If the Organ using this Move is your _Sinuous Tentacle Tongue_, add **+1** to this Move's attempted Stimulation, and also gain **(LB) Craving**.

During your partner's turn, you may spend **1 Dominance Point** to force them to take an action, or _not_ take an action, of your choice. You may only do this once per turn.

> **Subspace** 
> _Passive_
>
> You have disadvantage on all Grace Checks and Sanity Checks. You have advantage on all Ardor Checks and Moxie Checks.
>
> At the beginning of your turn, if you are not Having An Orgasm, you may roll a Moxie Check. If the result of the Check is greater than your Craving value, you may remove this effect."""
    }


ambrosia : Feature
ambrosia =
    { name = "Ambrosia"
    , description = """If a Pairing between your _Veiny Futa Phallus_ and __an Organ not owned by you__ that is in the "**Mouth**" category exists at any point during your turn, and that turn is one in which you are **Having An Orgasm**, then at the moment your ejaculate enters their mouth, they compulsively swallow it and acquire the **Fixation**effect.


> **Fixation**
> _Passive_
>
> You have disadvantage on all actions that do not target the Organ that inflicted this effect. You cannot volitionally Unpair from the Organ that inflicted this effect.
>
> At the beginning of your turn, you may roll a Sanity Check. If the result of the Check is greater than your Craving value, you may remove this effect.

While the one who swallows your Ambrosia remains Fixated, you gain an extrasensory perception of their body and sexual state.

If, at the beginning of _their_ turn, they fail their Sanity Check to remove the Fixation effect, or do not attempt to remove the Fixation effect, they must give you access to all of the information on their Persona Card and Organ Cards for a full round, until the start of their _next_ turn."""
    }


gardenKeeper : Feature
gardenKeeper =
    { name = "Garden Keeper"
    , description = """At the beginning of your turn, you may optionally **Regenerate Stamina** in an amount equal to the result of a **Fitness Check** instead of your base **Fitness Score**. If you elect to roll, drain a number of points equal to the number on the **d10** from your **Craving**.

You also permanently gain access to these two Moves:

> **Hunger of Inspiration** (Grind) [Squishes/Grips/Penetrates] | CT **0** |
> _Reaction_
>
> If the target of this move has the **Subspace** effect, gain **Craving** equal to the **Stamina** spent on the Move that this Move is contesting.

> **Pleasing Arrangement** (Tease) [Penetrates] | CT **20** |
> _Indulgent_
>
> If you have at least **1** Dominance Point, expend **1 Dominance Point** to force the target of this Move to use an available __reaction__ Move of your choice, provided the target has the necessary Stamina.
>
> If the target has no __reaction__ Moves available, ignore this."""
    }


fairyFlight : Feature
fairyFlight =
    { name = "Fairy Flight"
    , description = """Manifest at will ethereal butterfly wings that give you **+5** on all **Grace Check**s, both within and outside sexual encounters.

These wings count as two __Occupied Appendages__ that support your weight and stabilize you, while they exist.

These wings also allow you to fly for a number of minutes equal to your **Fitness Score** multiplied by **10**. You recover flight-time at a rate of **(1 + Fitness)** minutes per minute of rest."""
    }


seedstalker : GendertropeRecord
seedstalker =
    { name = "The Seed Stalker"
    , description =
        "She is a creature of insidious beauty and the implacable urge to breed. Hers is the cunning instinct that lures in the unsuspecting and takes from them in the heat of passion. Her focused hunt for a full womb drives her to prowl and seduce the unwitting. But her carnal pragmatism is tempered by an unselfconscious pride, an earnest honesty toward those who are generous to her."
    , features =
        [ ( 1, primordialInstinct )
        , ( 2, prowlingBreeder )
        , ( 3, seductiveSuction )
        , ( 4, nestingWomb )
        , ( 5, broodmother )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Shiny Kissable Lips"
        , hands "Slender Elegant Hands"
        , breasts "Perky Marshmallow Tits" "Cleft"
        , hips "Tight Supple Ass"
        , yonic "Sultry Suckling Meatsleeve"
        , legs "Long Shapely Legs" "Tight"
        , { emptyOrgan
            | name = "Serpentine Thirsting Tailthroat"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 2
            , canGrip = True
            , canEnsheathe = True
            , canPenetrate = True
          }
        ]
    , icon = Nothing
    }


primordialInstinct : Feature
primordialInstinct =
    { name = "Primordial Instinct"
    , description = """When you attempt a __contested__ **Repositioning Maneuver**, add **LBd4** to the result of your **Grace Check**."""
    }


prowlingBreeder : Feature
prowlingBreeder =
    { name = "Prowling Breeder"
    , description = """Your womb now contains a Pool of **Breeding Points** with a capacity equal to your _Sultry Suckling Meatsleeve_'s **Erogeny**.

While any Organ in the "**Phallic Genital**" Category is Paired with your _Sultry Suckling Meatsleeve_ when that Organ's owner begins their turn and determines if they are Having An Orgasm, if they are Having An Orgasm, you gain **1 Breeding Point** at that time.

You also permanently gain access to these three **Moves**:

> **Invasive Massage** (Grind) [Grips/Penetrates] | CT **5** |
>
> Drain **5 Craving** from yourself. Inflict the **Eager Release** effect to the target of this Move.

> **Torment The Tip** (Tease) [Grips/Ensheathes] | CT **15** |
>
> If your current number of **Breeding Points** is equal to or greater than the final **Stimulation** this Move delivers, expend a number of **Breeding Points** equal to that final Stimulation. Immediately reduce the **Sensitivity**, and increase the **Intensity**, of the target of this Move, by __that number__.
> 
> If this Move causes any **Overstimulation** to its target, the final **Sensitivity** gain from that Overstimulation is doubled.

> **Plundering Plunge** (Thrust) [Penetrates] | CT **0** |
>
> If the Organ using this Move is your _Sinuous Tentacle Tongue_, add **+1** to this Move's attempted Stimulation, and also gain **(LB) Craving**.

(If you did not replace your _Veiny Futa Phallus_, you may assign this Feature to your _Serpentine Thirsting Tailthroat_ instead. If you also did not replace your _Sinuous Tentacle Tongue_, the capacity for your **Breeding Points** is locked to **0**.)

> **Eager Release**
> _Passive_
>
> Subtract your **Craving** from the total when calculating your **Orgasm Threshold**.
> 
> At the beginning of your turn, if your **Intensity Points** are greater than your **Craving**, remove this effect."""
    }


seductiveSuction : Feature
seductiveSuction =
    { name = "Seductive Suction"
    , description = """If an Organ in the "**Phallic Genital**" Category attempts to **Unpair** from an Organ you own that [CanEnsheathe], and if you choose to __contest__ that Unpairing, the attempting Organ immediately receives __Thrusting Stimulation__ from your Ensheathing Organ at no extra cost to you. The amount of __Thrusting Stimulation__ they receive is equal to the total result of __their__ **Grace Check**, including any **Stamina** they spent to increase it.

The owner of the attempting Organ must then immediately roll an **Orgasm Sanity Check** __even if they are not Having An Orgasm__. If they fail their Orgasm Sanity Check, __you__ get an additional bonus to the result of your Grace Check that is equal to __their number of__ **Intensity Points**."""
    }


nestingWomb : Feature
nestingWomb =
    { name = "Nesting Womb"
    , description = """You may freely convert your **Breeding Points** into **Stamina** at any time.

You also gain access to these two Moves:

You also permanently gain access to these two Moves:

> **Primal Thirst** (Thrust) [Ensheathes] | CT: **0** | |
> _Reaction_
>
> If the target of this move has the **Eager Release** effect, gain as much **Craving** as the **Stamina** you spend on this contested Move.

> **Slithering Snuggle** (Grind) [Squishes] | CT **10** |
> _Indulgent_
>
> If you have at least 4 Breeding Points, drain 1d4 Sensitivity from yourself.
> If you have at least 6 Breeding Points, drain 2d4 Sensitivity from yourself.
> If you have at least 8 Breeding Points, drain 3d4 Sensitivity from yourself."""
    }


broodmother : Feature
broodmother =
    { name = "Broodmother"
    , description = """Outside of an Encounter, you may expend **5 Breeding Points** to give birth to an _Autonomous Tentacle_.

> __**Autonomous Tentacle**__
> 
> FIT: **2** | GRC: **6** | ARD: **8** | PRW: **4** | SAN: **0** | MOX: **2**
> 
> Suckling Tail-Pussy [CG][CP][CE] | [IG] (Prehensile)
> 
> Spurting Cock-Tail [CG][CP] | [IG] (Prehensile)
> 
> __Moves:__
> - Stroke
> - Squeeze
> 
> Somewhere between a meter and two meters of smooth tubular flesh, yonic at one end, phallic at the other end, that does its best to act as a pipe that pumps ejaculate through itself. When several of these are in physical contact with each other and only each other, they will instinctively link up and evenly distribute any collected ejaculate amongst the swarm, slowly leeching energy from the ejaculate's alchemical charge, forming braided rings of continuous flow. If uninterrupted, these braided rings can sometimes form useful structures.

The Autonomous Tentacle is an unintelligent orgasmophage that will, left to its own devices, mindlessly seek to extract semen from phallic genitals and deposit that semen in yonic genitals, preferably at the same time.

You may give one of your _Autonomous Tentacles_ a mental command by succeeding at a **DC 8 Moxie Check**. You must roll a **Moxie Check** for every individual action you order your tentacle-spawn to perform during an Encounter, but one Check can cover a complex order while outside of an Encounter.

In an Encounter, __only the first order you give is free__. The second order costs **1 Stamina**, the third order costs **2 Stamina**, the fourth order costs **3 Stamina**, and so on. The Stamina cost is paid even if you fail the Moxie Check. After ending an Encounter, this cost resets.

There is no hard limit on how many _Autonomous Tentacles_ you may have, but the emergent consequences of having a lot of _Autonomous Tentacles_ following you around are what they are."""
    }


flower : GendertropeRecord
flower =
    { name = "The Flower"
    , description = "She is an object of tempting beauty and creative growth, decorative and useful in equal measure. Her body is offered freely to any who would take her, for her longing to be plucked and kept and tended by a worthy gardener runs deep. But she is fearless, and is not one who trades herself for safety. Only for joy."
    , features =
        [ ( 1, sinfulNectar )
        , ( 2, preciousObject )
        , ( 3, alchemicalWomb )
        , ( 4, honeypot )
        , ( 5, rootedPetals )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Soft Rosen Lips"
        , hands "Delicate Girly Hands"
        , breasts "Soft Succulent Boobies" "Valley"
        , hips "Plush Bubble Butt"
        , yonic "Yielding Silken Quim"
        , legs "Cute Limber Legs" "Thicc"
        ]
    , icon = Nothing
    }


sinfulNectar : Feature
sinfulNectar =
    { name = "Sinful Nectar"
    , description = """When a partner's Organ is paired with an Organ belonging to this Flower that has [IsPenetrable], that partner's Organ receives a bonus to Erogeny equal to this Flower's Ardor."""
    }


preciousObject : Feature
preciousObject =
    { name = "Precious Object"
    , description = """You permanently gain access to these two Moves:

> **Irresistible Squirm** (Tease) [Squishes] | CT: **20** |
>
> If you have the **Subspace** effect, you may drain **10 Craving** from yourself to apply the **Shiver** effect to the target of this Move.

> **Kegel Quiver** (Grind) [Ensheathes] | CT: **0** |
>
> If you have the **Fixation** effect, add **+1** to this Move's attempted Stimulation.
>
> If this Move deals Stimulation equal or greater than the Erogeny of the Organ using this Move, gain **(LB) Craving**.

_Kegel Quiver_ does not require you to target the Organ you are Fixated on, or use the Organ that is Paired to said Fixation target.

> **Shiver** 
> _Passive_
>
> You have disadvantage on Prowess Rolls (but not Prowess Checks). When using a Move, you must spend _all_ of your available Stamina on the attempted Stimulation for that Move.
>
> At the beginning of your turn, you may roll a Prowess Check. If the result of the Check is greater than your Craving value, you may remove this effect."""
    }


alchemicalWomb : Feature
alchemicalWomb =
    { name = "Alchemical Womb"
    , description = """Your womb now possesses three (**3**) **Seed Cache**s.

Once per Encounter, if your _Yielding Silken Quim_ is Paired with an Organ in the "**Phallic Genital**" Category while the owner of that Organ is **Having An Orgasm**, you may fill one of your empty **Seed Caches** with their ejaculated fluid. Note the _alchemical property_ of each fluid since that will be important later. Consult the __Womb Alchemy Crafting__ section to determine a fluid's alchemical property.

> Once per Encounter, if you have at least **1 Seed Cache** filled, you may empty **1** Seed Cache, destroying the contents, to give an **Ability Score** of your choice a **+10** bonus. This bonus ends when the Encounter ends.

Once you have filled all three of your womb's Seed Caches, those contents combine to germinate and gestate an Egg. This takes one full round, from the start of your turn to the start of your following turn. See the __Womb Alchemy Crafting__ section for the Egg's possible Alchemical effect(s).

Once you have gestated an Egg, it remains in your womb until the next time you are Having An Orgasm while your _Yielding Silken Quim_ is Unpaired. Once it emerges from your vagina, it can be interacted with as a plain [CS][IG][IE] Toy. It will not break unless it's Alchemical effect is triggered deliberately."""
    }


honeypot : Feature
honeypot =
    { name = "Honeypot"
    , description = """When a partner successfully Pairs one of their Organs to one of your Organs with a **Firm Maneuver** or a **Violent Maneuver**, you gain **Craving** equal to your **Level Bonus**.

You permanently gain access to these Moves:

> **Melting Tremble** (Thrust) [Squishes/Ensheathes] | CT **15** |
> _Reaction_
>
> If you are **Having An Orgasm** (and pass your Orgasm Sanity Check), inflict the **Heartburst** effect on the owner of the Organ this Move is targeting.

> **Savoring Slide** (Grind) [Ensheathes] | CT **5** |
> _Indulgent_
>
> If you do not have the **Subspace** effect, drain **5** Craving from yourself."""
    }


rootedPetals : Feature
rootedPetals =
    { name = "Rooted Petals"
    , description = """Manifest a bushel of lush, slippery, floral vines from the nearest solid inanimate surface. These Root vines function as one Organ with a compatible Appendage for each of your bilateral Organ Appendages, and **10** Contour.

Upon summoning, the vines attempt to bind all of your bilateral Organs in inescapable coils of twisting floral color. However, if any of those Organs have pre-existing Pairings at the time of summoning, the Roots will falter and fail to finish manifesting. To **Take Root**, you must first Unpair all Organs with 'Left' and 'Right' Appendages.

Should the manifestation complete, you become bound into the presentation of a literal flower, with your body as the petals. When you unlock this Feature, choose and write down the specific pose in which your Roots will bind you.

While you are bound in your Roots, you cannot resist any Pairing changes to those of your Organs that are not already Paired to the Root vines, and you cannot make any Pairing changes yourself, but you are also __completely immune to any status effect(s) that you do not wish to affect you__. In addition, the Root vines will resist any attempt to displace its Pairings, with Violent Maneuvers that use your Ability Scores and rolls but do _not_ cost you any Stamina.

The limbs bound by your Roots count as Occupied.

You may dismiss your Roots at the end of your turn, freeing yourself and allowing the Root vines to vanish.

Outside an Encounter, these Roots can be summoned at any distance within line of sight, and will reach across the intervening distance to grab their petals and pull you through space to wherever they spawned from. This can be done a number of times per minute equal to your Moxie score."""
    }


housepet : GendertropeRecord
housepet =
    { name = "The Housepet"
    , description = "She is the most domesticated and affectionate of companions, loving attention and praise. Her gentle nature and clever creativity make her crave all things cozy, but she is also fascinated by, and drawn to, the creepy and strange. She would sooner befriend, and be happier belonging to, alien monsters than her own peers."
    , features =
        [ ( 1, skinHunger )
        , ( 2, snuggleSavant )
        , ( 3, ropeBunny )
        , ( 4, cleverKitten )
        , ( 5, limitless )
        ]
            |> Dict.fromList
    , organs =
        [ { emptyOrgan
            | name = "Cute Kitty Ears"
            , type_ = Other
            , contour = 0
            , erogeny = 4
            , isSquishable = True
            , isGrippable = True
          }
        , mouth "Soft Rosen Lips"
        , hands "Delicate Girly Hands"
        , breasts "Soft Succulent Boobies" "Valley"
        , hips "Plush Bubble Butt"
        , phallic "Adorable Twitching Girldick"
        , legs "Cute Limber Legs" "Thicc"
        ]
    , icon = Nothing
    }


skinHunger : Feature
skinHunger =
    { name = "Skin Hunger"
    , description = """You gain a **Skinship Bonus** to the **Erogeny** of all of your **Organs**, equal to half of your **Sanity Score** (rounded down) plus the total number of existing [Squish] Pairings in which any of your Organs are involved. However, if either you or your partner(s) are wearing any __Clothing__ items, or if no such [Squish] Pairings exist, your Skinship Bonus is **0**.

When a **Move** is used on you by a partner, the **Reciprocal Stimulation** caused by that Move is increased by your **Skinship Bonus**.

When you use a **Move** on a partner, add your **Skinship Bonus** to any **Reciprocal Stimulation** that __you__ receive."""
    }


snuggleSavant : Feature
snuggleSavant =
    { name = "Snuggle Savant"
    , description = """You permanently gain access to these two Moves:

> **Cozy Clench** (Thrust) [Grips/Ensheathes] | CT: **5** |
>
> If you have the **Eager Release** effect:
- Add **+1** to this Move's attempted Stimulation.
- Drain **Craving** from yourself equal to the **Stamina** spent on this Move, then also drain __the amount of drained Craving__ from your **Satiation**.

> **Eager Undulation** (Grind) [Penetrates/Ensheathes] | CT: **0** |
>
> If your **Craving** is less than the total **Erogeny** of the Organ using this Move, increase your **Craving** value to match that total **Erogeny** value.

> **Eager Release**
> _Passive_
>
> Subtract your **Craving** from the total when calculating your **Orgasm Threshold**.
> 
> At the beginning of your turn, if your **Intensity Points** are greater than your **Craving**, remove this effect."""
    }


ropeBunny : Feature
ropeBunny =
    { name = "Rope Bunny"
    , description = """Once per Encounter, choose __one__ of:
- **Ardor**
- **Prowess**
- **Sanity**
- **Moxie**

For the remaining duration of that Encounter, the **Ability Score** you chose is increased by your **Bondage Bonus**.

Your **Bondage Bonus** is at any given time equal to the highest **Bondage Rating** of any Toy that has Paired and remains Paired with any of your Organs, plus an additional **+1** for each additional Organ that is Paired with a Toy that has a **Bondage Rating** higher than your base **Grace Score**.

Your Bondage Bonus does not include the Bondage Rating of any Toys that you are _wielding_."""
    }


cleverKitten : Feature
cleverKitten =
    { name = "Clever Kitten"
    , description = """You may use your **Sanity Score** instead of your **Moxie Score** when rolling **Moxie Checks**.

You also gain access to these two Moves:

> **Compliant Fugue** (Tease) [Penetrates/Ensheathes] | CT **20** |
> _Reaction_
>
> If you already have the **Subspace** effect, you immediately gain the **Liquid Bliss (Tease)** effect.

> **Humping Cling** (Grind) [Penetrates/Ensheathes] | CT **0** |
> _Indulgent_
>
> Add your **Skinship Bonus** to both the Stimulation you receive from this Move _and_ to the Stimulation received by the target of this Move.

> Liquid Bliss (Tease)
> _Passive_
> 
> You cannot gain **Sensitivity**** from **Overstimulation** of the __Tease__ Stimulation Type.
> 
> At the beginning of your turn, remove this effect."""
    }


limitless : Feature
limitless =
    { name = "Limitless"
    , description = """You now roll a **d20** instead of a **d10** when rolling **Sanity Checks**. This does not apply to using your Sanity Score for Moxie Checks.

When you receive __positive__ **Stimulation**, if your **Craving** value is equal to or greater than the total **Stimulation** you receive, add any **Overstimulation** you receive to your **Arousal**.

When you receive __negative__ **Stimulation**, roll a **Sanity Check**. If the result is greater than the total absolute unsigned value of the negative Stimulation, you immediately gain the **Masochism** effect. Masochism does not cause the Pain to count as positive Stimulation for the purposes of this Feature.

Outside of an Encounter, once per **Full Recovery**, you may attempt a **DC 30 Sanity Check**. If you pass the check, you may instantly __teleport__ under your own power to anywhere you have ever been before or anywhere the GM deigns to send you."""
    }


vixen : GendertropeRecord
vixen =
    { name = "The Vixen"
    , description = "She is one who embodies the predator, the explorer, and the protector all at once. Her whims and her primal hungers drive her to hunt and slake, to frolic and play, to guard and comfort, forever seeking excitement. But her mercurial heart is innocent and soft, gentle and always welcoming... just like her bosom."
    , features =
        [ ( 1, milkMommy )
        , ( 2, savageRavisher )
        , ( 3, fluffyTale )
        , ( 4, wildAbandon )
        , ( 5, tittyPhysics )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Hot Hungry Maw"
        , hands "Deft Nimble Hands"
        , breasts "Mamerous Milk Melons" "Valley"
            |> withAppendage
                { emptyAppendage
                    | name = "Two Nipples At Once"
                    , isSquishable = True
                    , isGrippable = True
                }
        , hips "Bouncy Runner Rump"
        , yonic "Juicy Nether Cleft"
        , legs "Beastly Hunter Legs" "Tight"
        ]
    , icon = Nothing
    }


milkMommy : Feature
milkMommy =
    { name = "Milk Mommy"
    , description = """You lactate during your orgasms.

If an Organ in the "**Mouth**" Category is Paired with either or both of your _Mamerous Milk Melons_ at the start of a turn in which you are **Having An Orgasm**, the owner of that Organ imbibes the __ejaculate__d milk and is afflicted by the **Swoon** effect.

> **Swoon**
> _Trigger_
>
> When one of your Organs is the target of a **Repositioning Maneuver**, you cannot __contest__ that Maneuver.
>
>This effect is removed after triggering **1** time.

This feature cannot inflict __multiple stacks__ of **Swoon** on a partner. If the affected partner already has a **Swoon** effect, they do not gain an additional one."""
    }


savageRavisher : Feature
savageRavisher =
    { name = "Savage Ravisher"
    , description = """You permanently gain access to these three Moves:

> **Suckling Slam** (Thrust) [Ensheathes] | CT: **0**  |
>
> If the target of this Move is **Having An Orgasm**, they gain a number of additional **Intensity Points** equal to either your **Fitness**, or the **Erogeny** of the Organ this move is targeting, whichever is less. And you gain **(LB) Craving**.

> **Nurturing Nuzzle** (Grind) [Squishes] | CT: **10** |
>
> Roll a **Moxie Check**. The **Sensitivity** of the target of this Move is immediately reduced by the result of the Check.

> **Fey Tickle** (Tease) [Grips] | CT: **20** |
>
> You may drain **5 Craving** from yourself to demand to know the current **Sensitivity** of the target of this Move.
>
> If this Move is used to inflict pleasure (positive Stimulation), and if the attempted Stimulation is greater than or equal to the target's Sensitivity, add **+1** to the attempted Stimulation.
>
> If this Move is used to inflict pain (negative Stimulation), and if the (unsigned absolute value of the) attempted Stimulation is _less_ than the target's Sensitivity, drain **5 Craving** from yourself to apply the **Heartburst** effect to the target of this Move.

In addition, while you are **Having An Orgasm**, you may roll a Fitness Check instead of a Moxie Check or a Moxie Check instead of a Fitness Check, interchangeably.

> **Heartburst** 
> _Trigger_
> If you go from not Having An Orgasm to Having An Orgasm, immediately gain an additional **1d8 Intensity Points**.
>
> This effect is removed after triggering **1** time."""
    }


fluffyTale : Feature
fluffyTale =
    { name = "Fluffy Tale"
    , description = """Once per Encounter, you may choose one of your partner's **Organs**.

You may Occupy your _Hot Hungry Maw_ each turn with moaning and murmuring lustful and desirous compliments about that Organ into your partner's ear. Count each turn you do and have done this, and keep track of **that number**.

If ever the owner of the chosen Organ attempts to **Unpair** that Organ from one of your Organs, they must first roll a **Sanity Check**. If the result of the Sanity Check is less than **that number**, they cannot make the attempt, and must immediately make a **Move** with the chosen Organ instead."""
    }


wildAbandon : Feature
wildAbandon =
    { name = "Wild Abandon"
    , description = """At the beginning of your turn, you may roll an **Ardor Check**. You may drain _up to_ that many points of **Satiation** and add those points to your **Arousal** if and only if your **Arousal** is _not_ at maximum capacity..

You also permanently gain access to these Moves:

> **Discordant Wriggle** (Tease) [Grips/Ensheathes] | CT **5** |
> _Reaction_
>
> The **Satiation** of the owner of the Organ this Move is targeting is drained by an amount equal to the **Erogeny** of the Organ this Move is targeting. 

> **Rapacious Ride** (Grind) [Ensheathes] | CT **5** |
> _Indulgent_
>
> Roll a **Moxie Check**. If the result is greater than the Stimulation dealt by this Move, the owner of the Organ that this Move is targeting is affected by the **Fixation** effect."""
    }


tittyPhysics : Feature
tittyPhysics =
    { name = "Titty Physics"
    , description = """Once per turn, you may transform your _Mamerous Milk Melons_ into **Mamerous Mana Melons** or back again.

Your breasts become conduits and reservoirs of magical energy instead of milk, expanding to three times their previous size and granting you **varokinesis**, mental control over the inertial mass of your breast flesh.

The connective tissue inside your breasts and the skin of your breasts is protected by the concentrated mana. Reduce any pain inflicted on your breasts by (10 + Fitness). Reduce any Overstimulation inflicted on your breasts by your Fitness.

Your _Mamerous Mana Melons_ have an additional **+10** bonus to **Contour**.

Pairing attempts made by your _Mamerous Mana Melons_ have disadvantage. However, if your _Mamerous Milk Melons_ are already Paired with another Organ at the time of transformation into _Mamerous Mana Melons_, the owner of that Organ gains **2 Craving** and must make a **Prowess Check**. If the result of their Prowess Check is less than your (5 + Fitness), then until the end of their turn their Moves can target only your _Mamerous Mana Melons_.

Outside an Encounter, you may use the shiftable inertial mass of your breasts to double-jump, dash, and slow-fall."""
    }


buck : GendertropeRecord
buck =
    { name = "The Buck"
    , description = "He is one who lives in the moment and is captivated by passion. His earnest whimsy and innocent fascinations lead to carefree nights and a fondness for the unexpected, even for the dangerous, ever delighted by the thrill. But he yearns most deeply to be safe in the arms of someone stronger and kinder than himself."
    , features =
        [ ( 1, insatiable )
        , ( 2, recklessFool )
        , ( 3, whiteFountain )
        , ( 4, eagerPrey )
        , ( 5, youth )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Pretty Princely Pout"
        , hands "Clever Flexible Fingers"
        , chest "Slim Boyish Chest"
        , hips "Bitable Boy Butt"
        , phallic "Throbbing Meat Pole"
        , legs "Quick Springy Legs" "Tight"
        ]
    , icon = Nothing
    }


insatiable : Feature
insatiable =
    { name = "Insatiable"
    , description = """You permanently gain the **Insatiable** effect, which cannot be removed.
> **Insatiable**
> _Passive_
> 
> Your **Arousal** cannot drain to less than your **Ardor Score**.

Your erection is eternal, and you suffer no negative effects as a result of having a permanent erection."""
    }


recklessFool : Feature
recklessFool =
    { name = "Reckless Fool"
    , description = """You permanently gain access to these two Moves:

> **Starving Tongue-Kiss** (Thrust) [Squishes] | CT: **5** |
>
> If this Move causes any Overstimulation, you may drain **5 Craving** from yourself to grant yourself the **Lucid Fugue** effect.

> **Reckless Railing** (Thrust) [Penetrates] | CT: **0** |
>
> Roll an **Ardor Check**.
> - If the result is greater than the Stimulation dealt by this Move, you gain additional **Arousal** equal to the result.
> - If the result is less than or equal to the Stimulation dealt by this Move, the target of this move gains additional **Arousal** equal to the result.

In addition, you may add half of your **Craving** value to the result of any **Grace Check**s. If you do so, but fail the Grace Check anyway, you immediately acquire the **Shiver** effect.

> **Lucid Fugue**
> _Trigger_
>
> If you roll a standard **Orgasm Sanity Check**, and if you fail that Check, remove a number of **Intensity Points** equal to the result of the Check and add that many points to your **Craving**. This does not alter the outcome of the Check.
>
> This effect is removed after triggering **1** time."""
    }


whiteFountain : Feature
whiteFountain =
    { name = "White Fountain"
    , description = """While your _Bitable Boy Butt_ is Paired with an Organ that [CanPenetrate], you have a **-10** Modifier to your Orgasm Threshold and a **+10** bonus to the Erogeny of your _Throbbing Meat Pole_. In addition, your semen becomes magically-charged:

At the beginning of your turn, if you are **Having An Orgasm**, any Organ paired with your _Throbbing Meat Pole_ is soaked in your magically-charged semen, applying the **Epiphany** effect _both_ to the owner of that Organ as well as yourself.

> **Epiphany** 
> _Passive_
>
> If you have this effect when you end an Encounter by **satisfying** your partner, you gain **2** Euphoria Points instead of **1**."""
    }


eagerPrey : Feature
eagerPrey =
    { name = "Eager Prey"
    , description = """When you gain **Intensity Point(s)**, you may convert those points in an amount _up to_ your **Level Bonus** directly into **Arousal** instead.

You also permanently gain access to these Moves:

> **Rigid Repose** (Thrust) [Penetrates] | CT **20** |
> _Reaction_
>
> If and only if your **Arousal** is at maximum capacity, then at no additional cost, __double__ the total **Stimulation** dealt to the Organ this Move is targeting, both by this Move and by the Move this Move is a reaction to.

> **Needy Plunder** (Thrust) [Penetrates] | CT **15** |
> _Indulgent_
>
> If you are **Having An Orgasm**, add your current **Arousal** to the result of your Orgasm Sanity Check for this Move."""
    }


youth : Feature
youth =
    { name = "Captivated Heart"
    , description = """You may form a number of **Love Bond**s with other beings equal to your **Level Bonus**.

When someone confesses their strong positive feelings about you, to you, a spiritual attachment may form in your heart if the confession is sincere.

You and any individual you share a Love Bond with may draw strength from each other by sharing **Ability Scores**: when one of you rolls an Ability Check, either of you may use your Bonded's Ability Score instead of your own when determining the result of the Check.

Outside of an Encounter, those that share a Love Bond will be able to sense each other's location at all times.

If you already have as many Love Bonds as your Level Bonus when you receive a confession, you may break one of the existing Bonds to make room for the new one.

If you wish to break a Love Bond yourself, you may break one Love Bond per day, during a restful sleep. If you choose to do this, you may not also spend any Progression Points during that restful sleep.

Breaking a Love Bond has no on-going negative consequences. A repeated confession may (re)create a Love Bond as effectively as a first confession, given that the confession remains sincere."""
    }


fiend : GendertropeRecord
fiend =
    { name = "The Fiend"
    , description = "He is a cruel being of meticulous obsession and exacting desires. His esoteric pleasures are often strange or abstract, and he will craft them himself if he must, or if he prefers. But his implacable single-minded pursuit of his strange joys is intrinsically entwined with the intense empathy and fascination he feels for his living toys."
    , features =
        [ ( 1, dildonicSemblance )
        , ( 2, devilishDominator )
        , ( 3, bondageArtisan )
        , ( 4, masterCollector )
        , ( 5, theVoice )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Sensuous Knowing Lips"
        , hands "Steady Dexterous Hands"
        , chest "Firm Slender Pectorals"
        , hips "Chiseled Stately Ass"
        , phallic "Darkly Dreaming Dick"
        , legs "Fit Flexible Legs" "Tight"
        ]
    , icon = Nothing
    }


dildonicSemblance : Feature
dildonicSemblance =
    { name = "Dildonic Semblance"
    , description = """Choose one **Toy** that has a Summoner Rank of **10** or less, and which is not a __Fixture__. 

You are now able to summon that Toy freely, at no cost, without access to a Toybox."""
    }


devilishDominator : Feature
devilishDominator =
    { name = "Devilish Dominator"
    , description = """You permanently gain access to these three Moves:

> **Coaxing Curl** (Grind) [Grips/Penetrates] | CT: **0** |
>
> You may immediately view the state of all **Status Meters** on the owner, as well as all information on the **Organ Card** of, the Organ this Move is targeting. You may do this _before_ deciding how much attempted Stimulation this Move will deal.

> **Pernicious Pump** (Thrust) [Penetrates] | CT : **5** |
>
> You have advantage on the **Prowess Roll** for this Move if this Move is used to inflict pain (negative Stimulation).
>
> If the 'Organ' using this Move is _not_ one of your Feature-granted Toys, drain **5 Craving** from yourself.

> **Taste of Perfection** (Tease) [Squishes] | CT: **20** |
>
> Apply the **Perfectionism** effect to yourself.
>
> Drain **Stamina** from yourself equal to your **Level Bonus** _in addition to_ whatever you spend on Stimulation.

In addition, once per turn, if and only if you have the Perfectionism effect, you may roll a **Prowess Check**. If the result is greater than the **Summoning Rank** of any of your **Marked Toys** from a Toybox, you may Summon that Toy at **0** cost. This _does_ trigger and consume the Perfectionism effect.

> **Perfectionism** 
> _Trigger_
>
> Your next **Prowess Roll** or **Prowess Check** acts as though your **Prowess Score** is __twice__ its current value.
>
> This effect is removed after triggering **1** time."""
    }


bondageArtisan : Feature
bondageArtisan =
    { name = "Bondage Artisan"
    , description = """Choose another **Toy** with no restrictions. You are now also able to summon that Toy freely, at no cost, without access to a Toybox.

Both this Toy and the Toy granted by _Dildonic Semblance_ now have an additional effect:

At the beginning of each of your partner's turns, if any of your partner's Organs are Paired with either or both of your feature-granted Toys, and if that partner then determines that they are **Having An Orgasm** that turn, you roll a **Sanity Check**. Gain **Satiation** equal to the result."""
    }


masterCollector : Feature
masterCollector =
    { name = "Master Collector"
    , description = """At the beginning of your turn, you may roll a **Prowess Check**. You may move a number of points _up to_ the result of the Check from your **Arousal** to your **Craving**.

You also permanently gain access to these Moves:

> **Arresting Rhythm** (Thrust) [Squishes/Grips/Penetrates] | CT : **15** |
> _Reaction_
> 
> You may immediately view the **Craving** of the target of this Move.
> 
> If your Craving is greater than the Craving of the target of this Move, you may move an amount of points up to the amount of **Stamina** you spend on this Move, from your Craving to their Craving.

> **Implacable Focus** (Grind) [Penetrates] | CT : **10** |
> _Indulgent_
> 
> You may choose any one **Status Effect** that is currently affecting you and immediately remove that Status Effect.
> 
> If, and only if, the Stimulation you deal to yourself with this Move is _not_ __ideal__, if it would cause any Overstimulation or any Understimulation, drain **10 Craving** from yourself."""
    }


theVoice : Feature
theVoice =
    { name = "The Voice"
    , description = """Once per round, refreshing at the end of your turn, you may inflict **disadvantage** on any one **Ability Check** rolled by anyone within the reach of your voice.

Both inside and outside of an Encounter, you may issue a one-word command to an inanimate object or a subsapient being. If the GM allows, if physically able, the target of this command will obey it."""
    }


doll : GendertropeRecord
doll =
    { name = "The Doll"
    , description = "She is a blissful being of peaceful passivity and masochistic fatalism. Her only wish is to be treasured and tormented, teased and tantalized, in the hands of one worthy to own her, or even remake her. But her selfless wish to gratify her demanding master is tempered by her selfish wish for a life of mindless ecstasy."
    , features =
        [ ( 1, plugAndPlay )
        , ( 2, proudPlaything )
        , ( 3, exaltedPainslut )
        , ( 4, eternalDevotion )
        , ( 5, remoteNetworking )
        ]
            |> Dict.fromList
    , organs =
        -- TODO
        [ hips "Pliable Femme Hips" ]
    , icon = Nothing
    }


plugAndPlay : Feature
plugAndPlay =
    { name = "Plug And Play"
    , description = """Despite being (mostly) made of flesh, your Organs may be seamlessly, painlessly, and bloodlessly detached and swapped out for different ones.

__Sockets:__

The rules for Sockets go by what makes sense physically. In general:
- Any Organ that is Small can fit in any Socket.
- Any Organ that is Medium can fit in Medium and Large Sockets.
- Any Organ that is Large can only fit in in Large Sockets.

Determining the size of an Organ bends to the narrative and what makes physical sense therein, but in general:
- Mouths, Arms, and Genitals are Small.
- Breasts are Medium.
- Legs are Large.
- Additional Hips/Butts are larger than Large and thus too Large to attach to any of your Sockets.

You are not limited to bilateral Appendages in bilateral Sockets.

__Face Socket:__

You are not limited to Small Organs on your face.

However, if you attach a Medium Organ to your face Socket, you lose the ability to breathe and must have some kind of narrative justification for why you can remain conscious. Suffocation is not deadly, in the Starheart Lodge, but it _is_ usually entirely incapacitating.

If you attach a Large Organ to your face Socket, you lose both the ability to breathe and the ability to see. There are no mechanical rules for blindness, but narratively you do not have eyes.

Swapping out an Organ during an Encounter costs **Stamina** equal to your **Level Bonus**. A detached Organ follows the same rules as a **Toy**."""
    }


proudPlaything : Feature
proudPlaything =
    { name = "Proud Plaything"
    , description = """You permanently gain access to these two Moves:

> **Blissful Service** (Tease) [Squishes/Ensheathes] | CT: **0** |
>
> If the **Understimulation** dealt by this move is non-zero but less than your **Ardor**, you may grant yourself the **Subspace** effect.

> **Slavish Worship** (Grind) [Squishes/Grips] | CT: **15** |
>
> If you have the **Subspace** effect, and if this move deals non-zero positive Stimulation, you regain **Stamina** equal to the **Erogeny** of the Organ that this move is targeting.

In addition, if, at the end of your turn, you have the **Subspace** effect, you gain **Satiation** equal to your **Level Bonus** in addition to the results of your Aftermath."""
    }


exaltedPainslut : Feature
exaltedPainslut =
    { name = "Exalted Painslut"
    , description = """You permanently gain the **Masochism** effect, which cannot be removed.

> **Masochism** 
> _Passive_
>
> Take the absolute value, instead of the signed value, for any Stimulation you receive.
>
> Negative Stimulation you receive still counts as pain for any conditions that specify pain.

When you receive pain, roll a **Moxie Check**. If the result is greater than the unsigned absolute value of the pain, you are granted the **Subspace** effect."""
    }


eternalDevotion : Feature
eternalDevotion =
    { name = "Eternal Devotion"
    , description = """While you have the **Subspace** effect, your **Craving** can no longer fall lower than your **Satiation**. Keep your **Craving** filled such that it at least equals your **Satiation**.

You also gain access to these two Moves:


> **Tearful Tremble** (Tease) [Squishes/Grips/Ensheathes] | CT: **5** |
> _Reaction_
>
> If you have the Subspace effect, apply the Lubed effect to the target of this Move.

> **Existential Emptiness** (Tease) [Squishes/Ensheathes] | CT: **20** |
> _Indulgent_
>
> This Move cannot deal reciprocal Stimulation to the target.
> 
> For each point of **Stamina** you spend on this Move, drain one point of **Sensitivity**, one point of **Satiation**, and one point of **Craving**.
"""
    }


remoteNetworking : Feature
remoteNetworking =
    { name = "Wireless Anima"
    , description = """When you detach an Organ, you retain a connection to that Organ that allows you to continue feeling and acting through it.

A detached Organ:
- Keeps its Erogeny score and may receive Stimulation.
- May not attempt Pairings (unless held and used like a Toy).
- May resist attempted Pairings with disadvantage on the contested Grace Check and doubling of any Stamina costs.
- May, if Paired, use Moves at twice the non-detached Stamina cost.

If you do _not_ at the time have the **Subspace** effect, you may spend **1 Ichor Point** to sever your connection to one of your detached Organs."""
    }


junglePrince : GendertropeRecord
junglePrince =
    { name = "The Jungle Prince"
    , description = "He is an innocent beast, wild and free. His nurtured simplicity defines both his unhesitating integrity as well as his profound lack of inhibition. But though he thinks like an animal, his bestial nature is one of curious gentleness and loyal protectiveness."
    , features =
        [ ( 1, primalScent )
        , ( 2, nobleSavage )
        , ( 3, naturalTalent )
        , ( 4, carnalClarity )
        , ( 5, arborealAcrobat )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Hot Hungry Maw"
        , hands "Deft Nimble Hands"
        , chest "Rippling Manly Pecs"
        , hips "Bouncy Runner Rump"
        , phallic "Untamed Bitch Breaker"
        , legs "Beastly Hunter Legs" "Tight"
        ]
    , icon = Nothing
    }


primalScent : Feature
primalScent =
    { name = "Primal Scent"
    , description = """You may know the current **Arousal** and the current **Sensitivity** of any of your partners while you are able to __breathe__.

In addition, at the end of your turn, if a partner's **Arousal** is greater than your **Arousal**, you gain **Arousal** equal to your **Level Bonus**."""
    }


nobleSavage : Feature
nobleSavage =
    { name = "Noble Savage"
    , description = """Once per round, when you complete a __Gentle__ **Repositioning Maneuver** with an unresisting partner, you gain **Stamina** equal to your **Level Bonus**.

You also permanently gain access to these three **Moves**:

> **Sensual Exploration** (Tease) [Squishes/Grips] | CT: 0 |
> 
> Roll a number of **d4** equal to the **Stamina** spent on this Move.
> 
> For each d4 result, the target of this Move:
> - **1**: Increases their **Sensitivity** by 1.
> - **2**: Increases their **Arousal** by 1.
> - **3**: Decreases their **Sensitivity** by 1.
> - **4**: Decreases their **Satiation** by 1.
> 
> You gain **Craving** equal to the __number of__ **d4**s displaying a **4**.

> **Rutting Ravish** (Thrust) [Penetrates] | CT: 15 |
> 
> Drain **5 Craving** from yourself.
> 
> The owner of the Organ that this Move is targeting gains **1** stack of the **Ahegao** effect __for every **2__ Intensity Points** inflicted on them by this Move.

> **Playful Prurience** (Grind) [Squishes/Grips/Penetrates/Ensheathes] | CT: 5 |
> 
> Roll an Ardor Check.
> - If the result is greater than the Arousal of the target of this Move, the target of this Move gains the **Heartburst** effect.
> - If the result is less than the Arousal of the target of this Move, you gain the **Heartburst** effect.
> - If the result and the target's Arousal are equal, both of you gain the **Heartburst** effect."""
    }


naturalTalent : Feature
naturalTalent =
    { name = "Natural Talent"
    , description = """Once per turn, you may choose one **Organ Category**.

During that turn, whenever you use a **Move** through an **Organ** that is in your chosen **Category**, that Organ gains a bonus to its **Contour** that is equal to your **Grace Score**.

If you and your partner are both __feeling Innocent__ at the end of your turn, you and they may each add __your__ Grace Score to the results of each of your **Temperament Moxie Checks** in the moments before your own Temperament lapses. This bonus does not persist beyond the end of your turn."""
    }


carnalClarity : Feature
carnalClarity =
    { name = "Carnal Clarity"
    , description = """When you decide you are __feeling **Innocent**__, roll your Temperament Moxie Check with __advantage__.

You also gain access to these two Moves:

> **Intuitive Synchronicity** (Grind) [Penetrates/Ensheathes] | CT: 10 |
> _Reaction_
> 
> Compare your **Sensitivity** to the **Sensitivity** of the partner using the Move that this Move is contesting. Whomsoever has __less or equal__ Sensitivity to the other gains the **Burning Throb** effect.

> Mating Frenzy (Thrust) [Penetrates] | CT: 0 |
> _Indulgent_
> 
> If you are not already **Having An Orgasm**, and if the target of this Move _is_ already **Having An Orgasm**, you gain the **Eager Release** effect.

> **Burning Throb**
> _Passive_
> 
> > Your **Intensity Points** persist through turns even if you are not Having An Orgasm.
> 
> At the beginning of your turn, roll a Prowess Check. If the result of the Check is greater than your accumulated Intensity Points, you may remove this effect.
> 
> This effect is also removed if you are Having An Orgasm.

> **Eager Release**
> _Passive_
>
> Subtract your **Craving** from the total when calculating your **Orgasm Threshold**.
> 
> At the beginning of your turn, if your **Intensity Points** are greater than your **Craving**, remove this effect."""
    }


arborealAcrobat : Feature
arborealAcrobat =
    { name = "Arboreal Acrobat"
    , description = """When navigating in zero-gravity, traversing complex terrain, or attempting any other feat of __mobility__ that would depend on your **Fitness Score** or your **Grace Score**, your **Basic Competency** is instead the __sum__ of your **Fitness Score** _and_ your **Grace Score**.

In addition, you have __advantage__ on all mobility-related **Ability Checks** based on that __combined sum__.

During an Encounter, you may use that __combined sum__ in place of your **Prowess Score**. However, if you do so, your remaining **Stamina** drains to **0** immediately after completing the action you used it for."""
    }


firecracker : GendertropeRecord
firecracker =
    { name = "The Firecracker"
    , description = "She is a feisty and irrepressible seeker of validation and selfish satisfaction, both impulsive and exacting. Her greatest obstacle will forever be only herself, and she knows it. But she embodies a yearning to be swept away into thoughtless passion, to have the self-defeating control she clings to wrested from her by one who sees the true her underneath."
    , features =
        [ ( 1, defiantThrill )
        , ( 2, invitingBlush )
        , ( 3, terrificTightness )
        , ( 4, breathlessBrat )
        , ( 5, blazeDancer )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Pretty Princess Pout"
        , hands "Clever Flexible Fingers"
        , { emptyOrgan
            | name = "Tasty Little Titties"
            , type_ = Breasts
            , contour = 5
            , erogeny = 4
            , canSquish = True
            , isSquishable = True
            , isGrippable = True
          }
            |> bilateral
        , hips "Bitable Little Butt"
        , yonic "Tight Love Tunnel"
        , legs "Quick Springy Legs" "Tight"
        ]
    , icon = Nothing
    }


defiantThrill : Feature
defiantThrill =
    { name = "Defiant Thrill"
    , description = """If you choose to __resist__ a partner's attempt to Pair one of their Organs to one of your Organs, and then __fail__ the resulting **contested Maneuver** such that the Pairing succeeds, you drain the result of your failed Grace Check from your **Satiation**."""
    }


invitingBlush : Feature
invitingBlush =
    { name = "Inviting Blush"
    , description = """You permanently gain access to these three Moves:

> **Taunting Touch** (Tease) [Squishes/Grips] | CT: **10** |
> _Reaction_
> 
> If this Move __overcomes__ the Move it is contesting, you gain the **Fascination (Thrust)** effect.

> **Wriggle Rub** (Grind) [Squishes/Grips/Penetrates/Ensheathes] | CT: **10** |
> _Reaction_
> 
> If this Move __overcomes__ the Move it is contesting, you gain the **Fascination (Tease)** effect.

> **Bodacious Bounce** (Thrust) [Penetrates/Ensheathes] | CT: **10** |
> _Reaction_
> 
> If this Move __overcomes__ the Move it is contesting, you gain the **Fascination (Grind)** effect.

When you use a **Move**, if you have the **Fascination** effect that matches the **Stimulation Type** of that Move, you may consume that **Fascination** effect to refund __all__ the **Stamina** you spend on that Move.

This does not allow you to attempt Stimulation of greater values than you have the Stamina to afford in the first place."""
    }


terrificTightness : Feature
terrificTightness =
    { name = "Terrific Tightness"
    , description = """At the end of your turn, you may inflict the **Insatiable** effect on the owner of any Organ in the "**Phallic Genital**" Category that is Paired with your _Tight Love Tunnel_.

After the effect is applied, you may know that partner's **Arousal**. If their **Arousal** is greater than your own **Arousal**, you acquire the **Shiver** effect on yourself."""
    }


breathlessBrat : Feature
breathlessBrat =
    { name = "Breathless Brat"
    , description = """If your **Arousal** is at maximum capacity, you may consume a **Fascination** effect of any type to use __Fantasy Surge__ as a free action. (This does not add Fantasy Surge to your Moves List.)

You also gain access to this Move:

> **Bottom Power** (Thrust) [Ensheathes] | CT: **5** |
> _Indulgent_
> 
> If you have the **Fascination (Thrust)** effect, it is consumed and you must double the **Intensity Points** you inflict on yourself with this Move."""
    }


blazeDancer : Feature
blazeDancer =
    { name = "Blaze Dancer"
    , description = """Thermal energy cannot cause you to become **Injured**. This does not affect the various sensations thermal energy can cause you to feel.

Outside of an Encounter, you may expend **1 Numinous Point** to summon a volume of smokeless flame under your psychokinetic control.

While your __volume of flame__ exists, every time you complete a **Full Recovery** you must roll a **d12** __signed difference__ plus your **Moxie Score**. If the final result of that roll is negative, your volume of flame expires and disappears. Otherwise, your volume of flame persists indefinitely.

Your volume of flame can take any shape, but the complexity of that shape is limited by your **Grace Score**.
- Grace **2** or better: 1 platonic solid.
- Grace **4** or better: 10 connected platonic solids.
- Grace **8** or better: Freehand shapes. Legible words.
- Grace **16** or better: Anatomically correct animated renderings.

Your volume of flame cannot stretch across a distance greater than a number of feet equal to your **Sanity Score**.

Some portion of your flame must remain near to you, within a number of feet equal to your **Prowess Score**.

As a result of the above, the maximum range your flame can reach is a number of feet equal to your **Sanity Score** plus your **Prowess Score**.

Attempting to Injure someone else by burning them with your flame requires that your flame remain in physical contact with that person for six uninterrupted seconds.

You also gain access to this Move:

> **Burning Graze** (Tease) [Any] | CT: 10 |
> 
> You may not use this Move if you do not have an active volume of flame under your control.
> 
> Roll a **Prowess Chec**k. The Difficulty Class is **30** minus the **Fitness Score** of the target of this Move.
> 
> If you fail the Prowess Check, the **Stimulation** dealt by this Move is __negative__ regardless of your intention."""
    }


demonQueen : GendertropeRecord
demonQueen =
    { name = "The Demon Queen"
    , description = "She is an exemplar of devoted obsession, her gleeful sadism in perfect balance with her enduring delight in the victory of her victims. She torments out of love, becoming both one's greatest fan and greatest foe. She is satisfied with no less. But ultimately, her wish to see those she torments flourish is her sincere and driving core."
    , features =
        [ ( 1, empoweredDevotion )
        , ( 2, captureConnoisseur )
        , ( 3, dildonicPuppeteer )
        , ( 4, salivatingSuccubus )
        , ( 5, amorousConjuration )
        ]
            |> Dict.fromList
    , organs =
        [ mouth "Sensuous Knowing Lips"
        , hands "Steady Dexterous Hands"
        , breasts "Luscious Looming Swells" "Valley"
        , hips "Chiseled Stately Ass"
        , yonic "Sultry Insidious Slit"
        , legs "Fit Flexible Legs" "Tight"
        ]
    , icon = Nothing
    }


empoweredDevotion : Feature
empoweredDevotion =
    { name = "Empowered Devotion"
    , description = """
You gain a +1 Bonus to your Prowess for every 10 Craving you have.

You gain a +1 Bonus to your Grace for every 10 Arousal you have."""
    }


captureConnoisseur : Feature
captureConnoisseur =
    { name = "Capture Connoisseur"
    , description = """Once per Toy within a given Encounter, whenever you successfully Pair a Toy that you are wielding to a partner's Organ, as bondage, you immediately gain Stamina equal to that Toy's Bondage Rating.

You also permanently gain access to these three Moves:

> Tantalizing Torment (Grind) [Squishes/Grips/Penetrates/Ensheathes] | CT: 20 |
> 
> This Move inflicts additional Intensity Points equal to the Understimulation dealt by this Move.

> Edge Dance (Tease) [Squishes/Grips/Penetrates] | CT: 10 |
> 
> If, after the Stimulation from this Move is resolved, the Arousal of the target of this Move is greater than the Orgasm Threshold of the target of this Move, but the target of this Move is not yet Having An Orgasm, the target of this Move gains the Edge effect.

> Naked Heart (Thrust) [No Pairing Requirement] | CT: 15 |
> _Abstract_
> 
> Using this Move must Occupy an Organ you own in the "Mouth" Category for an entire turn. That Organ must be Unpaired.
> 
> You cannot use any other Moves during the turn in which you use this Move, before or after. (This does not include preventing your use of a Reaction during your partner's turn.)
> 
> The target of this Move must also be bound by at least one Toy with a Bondage Rating.
> 
> The target of this Move drains an amount of Stamina equal to half of their Arousal, rounded up."""
    }


dildonicPuppeteer : Feature
dildonicPuppeteer =
    { name = "Dildonic Puppeteer"
    , description = """For any Toy that is already present in an Encounter, you may expend Craving equal to that Toy's Summoning Rank to wield that Toy as a proxy Organ Appendage for one action without being Paired with that Toy. Actions taken by an Animated Toy on your behalf also require you to expend Stamina as you would for actions with any other Organ.

If the Toy you wish to Animate is being wielded conventionally by someone other than you, you may still wield the Toy to use a Move on your behalf. However, causing that Toy perform a Pairing action (or Unpairing action) while a partner is in a position to contest that Pairing action is still subject to the Maneuver rules as with any other Organ.

Note that if you are the one bound by a Toy, you can, by Animating that Toy, cause that Toy to Unpair itself from you, bypassing it's Bondage Rating entirely."""
    }


salivatingSuccubus : Feature
salivatingSuccubus =
    { name = "Salivating Succubus"
    , description = """When your partner(s) roll against the Bondage Rating of a Toy you summoned, to escape the bondage of that Toy, and:
- Succeed: You gain an amount of Satiation equal to your Level Bonus.
- Fail: You drain an amount of Satiation equal to your Level Bonus.

You also gain access to these Moves:

Dominant Fervor (Tease) [Grips/Penetrates/Ensheathes] | CT: 10 |
Reaction

An amount of your Intensity Points that is equal to the Stamina you expend on this Move is converted back into Stamina, which you then gain. This cannot give you more Stamina than you have Intensity Points to convert.
Grinning Grope (Grind) [Squishes/Grips] | CT: 0 |
Indulgent

If this Move neither Overstimulates you nor Understimulates you, you gain the Perfectionism effect."""
    }


amorousConjuration : Feature
amorousConjuration =
    { name = "Amorous Conjuration"
    , description = """During a Full Recovery, you may choose a number of non-sapient entities from the Erogiary equal to your Level Bonus. Those choices become primed for conjuration, overwriting any that were previously primed.

Each of your primed minions may be conjured into existence, under your complete control, once. Also, you may only perform such a conjuration once per Encounter.

Once conjured, your minion lasts until your next Full Recovery, until you dismiss it, or until it has no extant Organ Pairings. Even outside of an Encounter, your conjured minion must remain in contact, meaning it must maintain a Pairing with, someone other than itself, at all times, or the conjuration ends."""
    }


eldritch : GendertropeRecord
eldritch =
    { name = "The Eldritch"
    , description = "It is phallic sexual aggression distilled into its purest essence. Its amorphous form is an unending manifestation of slippery penetrative seduction, insidious and encompassing. But it is a generous and proud lover, always eager to share, adore, and uplift."
    , features =
        [ ( 1, omnidextrousMutability )
        , ( 2, writhingMorphology )
        , ( 3, mirrorOfTsul )
        , ( 4, liquidHeart )
        , ( 5, madRevelation )
        ]
            |> Dict.fromList
    , organs =
        [ { emptyOrgan
            | name = "Birfurcated Bulbous Core"
            , type_ = Hips
            , contour = 1
            , erogeny = 2
            , canSquish = True
            , isSquishable = True
          }
        , { emptyOrgan
            | name = "Nimble Squirming Tendrils"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 2
            , canGrip = True
          }
        , { emptyOrgan
            | name = "Diligent Serpentine Dicks"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 8
            , canGrip = True
            , canPenetrate = True
            , isGrippable = True
          }
        , { emptyOrgan
            | name = "Grabby Yonic Throats"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 6
            , canGrip = True
            , canEnsheathe = True
          }
        , { emptyOrgan
            | name = "Meaty Nub Pads"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 2
            , canSquish = True
            , canGrip = True
          }
        , { emptyOrgan
            | name = "Engulfing Flesh Pockets"
            , type_ = Prehensile
            , contour = 4
            , erogeny = 2
            , canSquish = True
            , canEnsheathe = True
            , isSquishable = True
          }
        ]
    , icon = Nothing
    }


omnidextrousMutability : Feature
omnidextrousMutability =
    { name = "Omnidexterous Mutability"
    , description = """You have a Morph Capacity equal to 5 plus your Level Bonus.

Your total number of "Prehensile" Appendages is equal to your Morph Capacity. While this number must remain constant, you may, during your turn, freely vary which "Prehensile" Organ these Appendages belong to. When you alter a Paired "Prehensile" Appendage in this way, its new form must also have a compatible Pairing Attribute, in at least one direction, to maintain a Pairing.

When you use a "Prehensile" Organ to use a Move, if that Organ has multiple valid Pairings between its Appendages and the target's Appendages available, the Stimulation dealt is replicated across all valid Pairings instead of divided between them or delivered to one alone. This causes you, by having multiple Appendages paired, to multiply the Stimulation dealt to one target, or deal Stimulation to multiple targets simultaneously, at no added Stamina cost. The Reciprocal Stimulation you receive is based on the sum of all Stimulation dealt, but is still capped by your Organ's Erogeny.

All Moves used by your "Prehensile" Organs gain the ability to inflict a stack of the Ahegao effect. A stack of the Ahegao effect is inflicted if:
- The target of the Move is already Having An Orgasm and the turn in which they started that Orgasm is already ended.
- The target of the Move already has an amount of Intensity Points that is greater than their base Sanity Score.
- Your Level Bonus is not less than the number of Ahegao stacks the target already has. (Additional-stack infliction limit is equal to your Level Bonus.)"""
    }


writhingMorphology : Feature
writhingMorphology =
    { name = "Writhing Morphology"
    , description = """When a partner makes an attempt to Unpair from any of your Organs, that partner must use only their base Grace Score instead of rolling a Grace Check.

Your Engulfing Flesh Pockets may Pair with any Organ, and subsequently use Moves that [Ensheathe], regardless of that Organ's Pairing Attributes.

You also permanently gain access to these three Moves:

Coiling Fondle (Tease) [Grips] | CT: 10 |

Drain an amount of Stamina from the target of this Move equal to the total number of Appendages through which this Move is being used on that target.

Vigorous Violation (Thrust) [Penetrates] | CT: 5 |

Drain an amount of Sensitivity from the target of this Move equal to the total number of Appendages through which this Move is being used on that target.

Molesting Cling (Grind) [Squishes] | CT: 5 |

Drain an amount of Satiation from the target of this Move equal to the total number of Appendages through which this Move is being used on that target."""
    }


mirrorOfTsul : Feature
mirrorOfTsul =
    { name = "Mirror of Tsul"
    , description = """You are at all times privy to the Craving of your partner(s).

Once per turn, you may compare your Craving to that of your partner(s), and increase your Craving to match the highest Craving among them. If your Craving does, in fact, increase, you gain the Impassioned effect.

Impassioned
Trigger

If you roll and fail an Ability Check, take the difference between the Difficulty Class of the action and the failed result. If you have at least that much Craving, you drain that much Craving, and you succeed at the Ability Check instead of failing.

This effect is removed at the end of your turn."""
    }


liquidHeart : Feature
liquidHeart =
    { name = "Liquid Hea(r)t"
    , description = """All of your Organs permanently gain a toggleable Lubed effect, for an at-will +5 Bonus to any of your Contour scores, as well as the Contour scores of any Organs or Items you ever interact with.

You also gain access to this Move:

Decadent Flesh Cocoon (Grind) [Squishes/Ensheathes] | CT: 15 |
Indulgent

If the total number of Pairings through which this Move is being used is equal to or greater than the total number of Organs possessed by the target of this Move:
- If the target is Injured, they immediately cease to be Injured.
- You gain additional Arousal equal to the number of stacks of Ahegao on the target.
- Both you and the target drain an amount of Sensitivity equal to your Level Bonus.
- The target gains Stamina equal to half, rounded down, of the Stamina you spend on this Move."""
    }


madRevelation : Feature
madRevelation =
    { name = "Mad Revelation"
    , description = """At the beginning of your turn, you may grant the Epiphany effect to any partner(s) who at that time have the Ahegao effect. When you do, you gain Craving equal to the number of stacks of the Ahegao effect that the grantee has on them.

If at least one participant in an Encounter has the Epiphany effect when the Encounter ends with either your own Satisfaction or anyone's Numbness, that Satisfaction or Numbness is cleared away immediately afterward.

Outside of an Encounter, you may at any time expend LB Craving to speak aloud one non-compound sentence, even if you do not otherwise have the ability to speak.
- If that sentence is a subjective question, all who hear it will immediately know their own true answer.
- If that sentence is an objectively-true statement, all who hear it will know it to be true.
- If that sentence is an objectively-false statement, all who hear it know it to be false and will also become mentally Injured if it does not become true or is not made true within the following six seconds.
- If that sentence is none of the above, it is simply heard like normal speech."""
    }


gendertropeFromName : String -> Maybe Gendertrope
gendertropeFromName name =
    case name of
        "The Butterfly" ->
            Just Butterfly

        "The Seed Stalker" ->
            Just SeedStalker

        "The Flower" ->
            Just Flower

        "The Housepet" ->
            Just Housepet

        "The Vixen" ->
            Just Vixen

        "The Buck" ->
            Just Buck

        "The Fiend" ->
            Just Fiend

        "The Doll" ->
            Just Doll

        "The Jungle Prince" ->
            Just JunglePrince

        "The Firecracker" ->
            Just Firecracker

        "The Demon Queen" ->
            Just DemonQueen

        "The Eldritch" ->
            Just Eldritch

        _ ->
            Nothing


organTypeToIcon : OrganType -> Maybe Appendage -> ( IconVariant, Bool )
organTypeToIcon type_ appendage =
    ( case type_ of
        Mouth ->
            Icons.mouth

        Hands ->
            Icons.hands

        Breasts ->
            Icons.breasts

        Hips ->
            Icons.hips

        Yonic ->
            Icons.yonic

        Phallic ->
            Icons.phallic

        Legs ->
            Icons.legs

        Prehensile ->
            Icons.prehensile

        Other ->
            Icons.other
    , case appendage of
        Just { name } ->
            name == "Left"

        Nothing ->
            False
    )


organTypeToString : OrganType -> String
organTypeToString type_ =
    case type_ of
        Mouth ->
            "Mouth"

        Hands ->
            "Hands"

        Breasts ->
            "Breasts"

        Hips ->
            "Hips"

        Yonic ->
            "Yonic"

        Phallic ->
            "Phallic"

        Legs ->
            "Legs"

        Prehensile ->
            "Prehensile"

        Other ->
            "Custom"


organTypes : List OrganType
organTypes =
    [ Mouth
    , Hands
    , Breasts
    , Hips
    , Yonic
    , Phallic
    , Legs
    , Prehensile
    , Other
    ]
