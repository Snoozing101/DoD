module Character exposing (Character
                          , CharacterStat
                          , decrementCharacterStat
                          , incrementCharacterStat
                          , newCharacter
                          , updateCharacterStats
                          , statsToList
                          , classToString
                          )

import Array
import Dict exposing (Dict)


type CharacterStat
    = Strength
    | Vitality
    | Agility
    | Intelligence
    | Luck
    | Aura
    | Morality


characterStatToString : CharacterStat -> String
characterStatToString stat =
    case stat of
        Strength ->
            "Strength"

        Vitality ->
            "Vitality"

        Agility ->
            "Agility"

        Intelligence ->
            "Intelligence"

        Luck ->
            "Luck"

        Aura ->
            "Aura"

        Morality ->
            "Morality"


type CharacterClass
    = Wanderer
    | Cleric
    | Mage
    | Warrior
    | Barbarian


classToString : CharacterClass -> String
classToString class =
    case class of
        Wanderer ->
            "Wanderer"

        Cleric ->
            "Cleric"

        Mage ->
            "Mage"

        Warrior ->
            "Warrior"

        Barbarian ->
            "Barbarian"


type alias Character =
    { class : CharacterClass
    , name : String
    , stats : Dict String ( CharacterStat, Int )
    , experience : Int
    , statPoints : Int
    }


newCharacter : Character
newCharacter =
    Character Wanderer
        "Apathy"
        (Dict.fromList
            [ makeStatDictEntry Strength 0
            , makeStatDictEntry Vitality 0
            , makeStatDictEntry Agility 0
            , makeStatDictEntry Intelligence 0
            , makeStatDictEntry Luck 0
            , makeStatDictEntry Aura 0
            , makeStatDictEntry Morality 0
            ]
        )
        0
        0


makeStatDictEntry : CharacterStat -> Int -> ( String, ( CharacterStat, Int ) )
makeStatDictEntry characterStat val =
    let
        statString =
            characterStatToString characterStat
    in
    ( statString, ( characterStat, val ) )


baseStatModifier : Int -> Int
baseStatModifier stat =
    stat + 2


incrementCharacterStat : Character -> CharacterStat -> Character
incrementCharacterStat character characterStat =
    let
        currCharacter =
            character

        oldStats =
            currCharacter.stats

        updatedStats =
            Dict.update (characterStatToString characterStat) (Maybe.map incrementStat) oldStats

        updatedClass =
            updateClass updatedStats
    in
    { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints - 1, class = updatedClass }


incrementStat : ( CharacterStat, Int ) -> ( CharacterStat, Int )
incrementStat ( characterStat, num ) =
    ( characterStat, num + 1 )


decrementCharacterStat : Character -> CharacterStat -> Character
decrementCharacterStat character characterStat =
    let
        currCharacter =
            character

        oldStats =
            currCharacter.stats

        updatedStats =
            Dict.update (characterStatToString characterStat) (Maybe.map decrementStat) oldStats

        updatedClass =
            updateClass updatedStats
    in
    { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints + 1, class = updatedClass }


decrementStat : ( CharacterStat, Int ) -> ( CharacterStat, Int )
decrementStat ( characterStat, num ) =
    ( characterStat, num - 1 )


updateClass : Dict String ( CharacterStat, Int ) -> CharacterClass
updateClass stats =
    let
        intelligence =
            getStatValue stats Intelligence

        morality =
            getStatValue stats Morality

        aura =
            getStatValue stats Aura

        strength =
            getStatValue stats Strength

        vitality =
            getStatValue stats Vitality

        agility =
            getStatValue stats Agility
    in
    if intelligence > 6 && morality > 7 then
        Cleric

    else if intelligence > 8 && aura > 7 then
        Mage

    else if strength > 7 && morality > 5 && (strength + vitality) > 10 then
        Warrior

    else if strength > 8 && (vitality + agility) > 12 && morality < 6 then
        Barbarian

    else
        Wanderer


getStatValue : Dict String ( CharacterStat, Int ) -> CharacterStat -> Int
getStatValue stats characterStat =
    let
        statEntry =
            Dict.get (characterStatToString characterStat) stats
    in
    case statEntry of
        Just ( _, val ) ->
            val

        Nothing ->
            0


updateCharacterStats : Character -> List Int -> Character
updateCharacterStats character randomList =
    let
        ( statPoints, statList ) =
            splitRandomList randomList
    in
    { character | stats = updateStats character.stats statList, statPoints = statPoints, experience = 1 }


splitRandomList : List Int -> ( Int, List Int )
splitRandomList randomList =
    case randomList of
        statPoints :: statList ->
            ( statPoints, statList )

        [] ->
            ( 0, [] )


updateStats : Dict String ( CharacterStat, Int ) -> List Int -> Dict String ( CharacterStat, Int )
updateStats oldCharacterStats statList =
    let
        updater =
            setNewStatValue statList

        oldCharacterStatList =
            Dict.toList oldCharacterStats
    in
    List.indexedMap updater oldCharacterStatList
        |> Dict.fromList


setNewStatValue : List Int -> Int -> ( String, ( CharacterStat, Int ) ) -> ( String, ( CharacterStat, Int ) )
setNewStatValue statList index oldStat =
    let
        newArray =
            Array.fromList statList

        newValue =
            Maybe.withDefault 0 (Array.get index newArray)

        statString =
            Tuple.first oldStat

        statType =
            Tuple.first (Tuple.second oldStat)
    in
    ( statString, ( statType, baseStatModifier newValue ) )

statsToList : Dict String ( CharacterStat, Int) -> List (String, (CharacterStat, Int))
statsToList stats =
    Dict.toList stats
