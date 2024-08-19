module Persona.Data exposing (breasts, gendertropeIcon, gendertropeToRecord, hands, hips, legs, mouth, phallic, prehensile, yonic)

import Dict
import Element exposing (Element)
import Icons
import Types exposing (Feature, Gendertrope(..), GendertropeRecord, Organ)


gendertropeIcon : Gendertrope -> Element msg
gendertropeIcon gendertrope =
    case gendertrope of
        Butterfly ->
            Icons.butterfly

        Flower ->
            Icons.flower

        Vixen ->
            Icons.vixen

        Buck ->
            Icons.buck

        Fiend ->
            Icons.fiend

        Doll ->
            Icons.doll

        Custom _ ->
            Icons.custom


gendertropeToRecord : Gendertrope -> GendertropeRecord
gendertropeToRecord gendertrope =
    case gendertrope of
        Butterfly ->
            butterfly

        Flower ->
            flower

        Vixen ->
            vixen

        Buck ->
            buck

        Fiend ->
            fiend

        Doll ->
            doll

        Custom record ->
            record


emptyOrgan : Organ
emptyOrgan =
    { name = ""
    , contour = 0
    , erogeny = 0
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
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


breasts : String -> Organ
breasts name =
    { emptyOrgan
        | name = name
        , contour = 5
        , erogeny = 4
        , canSquish = True
        , isSquishable = True
        , isGrippable = True
    }


hips : String -> Organ
hips name =
    { emptyOrgan
        | name = name
        , contour = 1
        , erogeny = 4
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isGrippable = True
        , isPenetrable = True
    }


legs : String -> Organ
legs name =
    { emptyOrgan
        | name = name
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , isGrippable = True
    }


phallic : String -> Organ
phallic name =
    { emptyOrgan
        | name = name
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
        , contour = 4
        , erogeny = 2
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


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
        [ prehensile "Sinuous Tentacle Tongue"
        , hands "Slender Elegant Hands"
        , breasts "Perky Marshmallow Tits"
        , hips "Tight Supple Ass"
        , phallic "Veiny Futa Phallus"
        , legs "Long Shapely Legs"
        ]
    }


prehensileProficiency : Feature
prehensileProficiency =
    { name = "Prehensile Proficiency"
    , description = "When using an Organ with both [CanGrip] and [CanPenetrate] to make a Prowess Roll, you may make the roll twice and take the superior result."
    }


dominantExemplar : Feature
dominantExemplar =
    { name = "Dominant Exemplar"
    , description = """You now have a pool of **Dominance Points** with capacity: **3**. Dominance Points do not persist between Encounters and begin at **0**.

You also permanently gain access to these three **Moves**:

> **Assertive Grope** (Tease) [Grips] | CT **6** |
>
> If, and only if, the Stimulation dealt by this Move causes **0** Understimulation _and_ **0** Overstimulation, apply the **Subspace** effect to the target of this Move.

> **Wrecking Rut** (Thrust) [Penetrates] | CT **20** |
>
If this Move deals Stimulation equal to or greater than the target's Sanity score, _and_ if the target of this Move has the **Subspace** effect, gain **1 Dominance Point**.

> **Plundering Plunge** (Thrust) [Penetrates] | CT **0** |
>
> _You want that nectar, and it doesn't matter how deep you have to plunge in to taste it. Nowhere is safe from your tongue._
>
> If the Organ using this Move is your _Sinuous Tentacle Tongue_, add **+1** to this Move's attempted Stimulation, and also gain **1 Craving**.

During your partner's turn, you may spend **1 Dominance Point** to force them to take an action, or _not_ take an action, of your choice. You may only do this once per turn.

> **Subspace** _Passive_
>
> You have disadvantage on all Grace Checks and Sanity Checks. You have advantage on all Ardor Checks and Moxie Checks.
>
> At the beginning of your turn, if you are not Having An Orgasm, you may roll a Moxie Check. If the result of the Check is greater than your Craving value, you may remove this effect."""
    }


ambrosia : Feature
ambrosia =
    { name = "Ambrosia"
    , description = """If your partner's mouth is Paired with your _Veiny Futa Phallus_ while you are **Having An Orgasm**, they may roll a **Sanity Check**. If the result of the Check is not greater than your penis' Contour, or if they choose not to make the Check, they compulsively swallow your ejaculate and acquire the **Fixation** effect.


> **Fixation** _Passive_
>
> You have disadvantage on all actions that do not target the Organ that inflicted this effect. You cannot volitionally Unpair from the Organ that inflicted this effect.
>
> At the beginning of your turn, you may roll a Sanity Check. If the result of the Check is greater than your Craving value, you may remove this effect.

In addition, so long as the effect remains, you gain an extrasensory perception of their body and sexual state and may demand full access to all the information on their Persona Card and Organ Cards at any time."""
    }


gardenKeeper : Feature
gardenKeeper =
    { name = "Garden Keeper"
    , description = """At the beginning of your turn, you may roll a **Fitness Check**. You may drain _up to_ that many points of **Craving** and add those points to your **Stamina Die**.

You also permanently gain access to these two Moves:

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
    }


fairyFlight : Feature
fairyFlight =
    { name = "Fairy Flight"
    , description = """Manifest at will ethereal butterfly wings that give you **+5** on all **Grace Check**s, both within and outside sexual encounters.

These wings count as two Occupied Appendages that support your weight and stabilize you, while they exist.

These wings also allow you to fly for a number of minutes equal to your Fitness score multiplied by 10. You recover flight-time at a rate of (1 + Fitness) minutes per minute of rest."""
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
        , breasts "Soft Succulent Boobies"
        , hips "Plush Bubble Butt"
        , yonic "Yielding Silken Quim"
        , legs "Cute Limber Legs"
        ]
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
> If this Move deals Stimulation equal or greater than the Erogeny of the Organ using this Move, gain **1 Craving**.

_Kegel Quiver_ does not require you to target the Organ you are Fixated on, or use the Organ that is Paired to said Fixation target.

> **Shiver** _Passive_
>
> You have disadvantage on Prowess Rolls (but not Prowess Checks). When using a Move, you must spend _all_ of your available Stamina on the attempted Stimulation for that Move.
>
> At the beginning of your turn, you may roll a Prowess Check. If the result of the Check is greater than your Craving value, you may remove this effect."""
    }


alchemicalWomb : Feature
alchemicalWomb =
    { name = "Alchemical Womb"
    , description = """Your womb now possesses three (**3**) **Seed Cache**s.

Once per Encounter, if your _Yielding Silken Quim_ is Paired with a penis or other Ensheatheable Organ, that is able to ejaculate, while the owner of that Organ is Having An Orgasm, you may fill one of your empty **Seed Caches** with their ejaculated fluid. Note the _source_ of each fluid since that will be important later.

> Once per Encounter, if you have at least **1 Seed Cache** filled, you may empty **1** Seed Cache, destroying the contents, to give an **Ability Score** of your choice a **+10** bonus. This bonus ends when the Encounter ends.

Once you have filled all three of your womb's Seed Caches, those contents combine to germinate and gestate an Egg. This takes one full round, from the start of your turn to the start of your following turn. See the __Womb Alchemy Crafting__ section for the Egg's possible Alchemical effect(s).

Once you have gestated an Egg, it remains in your womb until the next time you are Having An Orgasm while your _Yielding Silken Quim_ is Unpaired. Once it emerges from your vagina, it can be interacted with as a plain [CS][IG][IE] Toy. It will not break unless it's Alchemical effect is triggered deliberately."""
    }


honeypot : Feature
honeypot =
    { name = "Honeypot"
    , description = """When a partner successfully Pairs one of their Organs to one of your Organs with a **Firm Maneuver** or a **Violent Maneuver**, you gain **1 Craving**.

You permanently gain access to these Moves:
> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
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
        , breasts "Mamerous Milk Melons"
        , hips "Bouncy Runner Rump"
        , yonic "Juicy Nether Cleft"
        , legs "Beastly Hunter Legs"
        ]
    }


milkMommy : Feature
milkMommy =
    { name = "Milk Mommy"
    , description = """You lactate during your orgasms.

If your partner's mouth is paired with either or both of your _Mamerous Milk Melons_ at the time, they imbibe the resulting milk and become more compliant.

As a consequence, your next Repositioning Maneuver succeeds automatically at 0 Stamina cost and cannot be resisted. Once used, this cannot be used again until your current orgasm ends and your next orgasm begins."""
    }


savageRavisher : Feature
savageRavisher =
    { name = "Savage Ravisher"
    , description = """You permanently gain access to these three Moves:

> **Suckling Slam** (Thrust) [Ensheathes] | CT: **0**  |
>
> If the target of this Move is **Having An Orgasm**, they gain a number of additional **Intensity Points** equal to either your **Fitness**, or the **Erogeny** of the Organ this move is targeting, whichever is less. And you gain **1 Craving**.

> **Nurturing Nuzzle** (Grind) [Squishes] | CT: **10** |
>
> Roll a **Moxie Check**. The **Sensitivity** of the target of this Move is immediately reduced by the result of the Check.

> **Fey Tickle** (Tease) [Grips] | CT: **20** |
>
> You may demand to know the current **Sensitivity** of the target of this Move.
>
> If this Move is used to inflict pleasure (positive Stimulation), and if the attempted Stimulation is greater than or equal to the target's Sensitivity, add **+1** to the attempted Stimulation.
>
> If this Move is used to inflict pain (negative Stimulation), and if the (unsigned absolute value of the) attempted Stimulation is _less_ than the target's Sensitivity, apply the **Heartburst** effect to the target of this Move.

In addition, while you are **Having An Orgasm**, you may roll a Fitness Check instead of a Moxie Check or a Moxie Check instead of a Fitness Check, interchangeably.

> **Heartburst** _Trigger_
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
    , description = """At the beginning of your turn, you may roll an **Ardor Check**. You may drain _up to_ that many points of **Craving** and add those points to your **Arousal**.

You also permanently gain access to these Moves:

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
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
        , hips "Bitable Boy Butt"
        , phallic "Throbbing Meat Pole"
        , legs "Quick Springy Legs"
        ]
    }


insatiable : Feature
insatiable =
    { name = "Insatiable"
    , description = """Your Arousal can never drop below your Ardor Score."""
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

> **Lucid Fugue** _Trigger_
>
> If you roll a standard **Orgasm Sanity Check**, and if you fail that Check, remove a number of **Intensity Points** equal to the result of the Check and add that many points to your **Craving**. This does not alter the outcome of the Check.
>
> This effect is removed after triggering **1** time."""
    }


whiteFountain : Feature
whiteFountain =
    { name = "White Fountain"
    , description = """While your _Bitable Boy Butt_ is Paired with an Organ that [IsEnsheatheable], you have a **-10** Modifier to your Orgasm Threshold and a **+10** bonus to the Erogeny of your _Throbbing Meat Pole_. In addition, your semen becomes magically-charged:

At the beginning of your turn, if you are **Having An Orgasm**, any Organ paired with your _Throbbing Meat Pole_ is soaked in your magically-charged semen, applying the **Epiphany** effect _both_ to the owner of that Organ as well as yourself.

> **Epiphany** _Passive_
>
> If you have this effect when you end an Encounter by **satisfying** your partner, you gain **2** Euphoria Points instead of **1**."""
    }


eagerPrey : Feature
eagerPrey =
    { name = "Eager Prey"
    , description = """When you gain **Intensity Point(s)**, you may convert those points in an amount _up to_ your **Level Bonus** directly into **Arousal** instead.

You also permanently gain access to these Moves:

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
    }


youth : Feature
youth =
    { name = "Youth!"
    , description = """You have advantage on all Fitness Checks, always."""
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
        , hips "Chiseled Stately Ass"
        , phallic "Darkly Dreaming Dick"
        , legs "Fit Flexible Legs"
        ]
    }


dildonicSemblance : Feature
dildonicSemblance =
    { name = "Dildonic Semblance"
    , description = """Choose one **Toy**. You are now able to summon that Toy freely, at no cost, without access to a Toybox."""
    }


devilishDominator : Feature
devilishDominator =
    { name = "Devilish Dominator"
    , description = """You permanently gain access to these three Moves:

> **Coaxing Curl** (Grind) [Grips/Penetrates] | CT: **0** |
>
> You may immediately view the state of all **Status Meters** on the owner, as well as all information on the **Organ Card** of the Organ this Move is targeting. You may do this _before_ deciding how much attempted Stimulation this Move will deal.

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

> **Perfectionism** _Trigger_
>
> Your next **Prowess Roll** or **Prowess Check** acts as though your **Prowess Score** is __twice__ its current value.
>
> This effect is removed after triggering **1** time."""
    }


bondageArtisan : Feature
bondageArtisan =
    { name = "Bondage Artisan"
    , description = """Choose another **Toy**. You are now also able to summon that Toy freely, at no cost, without access to a Toybox.

Both this Toy and the Toy granted by _Dildonic Semblance_ now have an additional effect:

If the owner of the Organ(s) that are Paired with either or both Toys is **Having An Orgasm**, and that owner is _not_ you, roll a **Sanity Check**. Gain **Satiation** equal to the result."""
    }


masterCollector : Feature
masterCollector =
    { name = "Master Collector"
    , description = """At the beginning of your turn, you may roll a **Prowess Check**. You may move a number of points _up to_ the result of the Check from your **Arousal** to your **Craving**.

You also permanently gain access to these Moves:

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
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

> **Masochism** _Passive_
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

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
    }


remoteNetworking : Feature
remoteNetworking =
    { name = "Remote Networking"
    , description = """When you detach an Organ, you retain a connection to that Organ that allows you to continue feeling and acting through it.

A detached Organ:
- Keeps its Erogeny score and may receive Stimulation.
- May not attempt Pairings (unless held and used like a Toy).
- May resist attempted Pairings with disadvantage on the contested Grace Check and doubling of any Stamina costs.
- May, if Paired, use Moves at twice the non-detached Stamina cost.

If you do _not_ at the time have the **Subspace** effect, you may spend **1 Ichor Point** to sever your connection to one of your detached Organs."""
    }
