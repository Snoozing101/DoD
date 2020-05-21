module Character exposing
    ( Character
    , CharacterStat(..)
    , CharacterClass(..)
    , Stat
    , decrementCharacterStat
    , getClassString
    , getName
    , getStatList
    , getStatPoints
    , getXP
    , incrementCharacterStat
    , init
    , setName
    , updateCharacterStats
    )

import Array
import Dict exposing (Dict)


type alias Stat =
    { name : String
    , stat : CharacterStat
    , value : Int
    }

type alias StatHolder =
    { stat : CharacterStat
    , value : Int
    , order : Int
    }

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


type Character
    = Character
        { class : CharacterClass
        , name : Maybe String
        , stats : Dict String StatHolder
        , experience : Int
        , statPoints : Int
        }


init : Maybe String -> Character
init characterName =
    Character
        { class = Wanderer
        , name = characterName
        , stats =
            Dict.fromList
                [ makeStatDictEntry Strength 0 0
                , makeStatDictEntry Vitality 0 1
                , makeStatDictEntry Agility 0 2
                , makeStatDictEntry Intelligence 0 3
                , makeStatDictEntry Luck 0 4
                , makeStatDictEntry Aura 0 5
                , makeStatDictEntry Morality 0 6
                ]
        , experience = 0
        , statPoints = 0
        }


makeStatDictEntry : CharacterStat -> Int -> Int -> ( String, StatHolder )
makeStatDictEntry stat value order =
    let
        statString =
            characterStatToString stat
    in
    ( statString, { stat=stat, value=value, order=order } )


baseStatModifier : Int -> Int
baseStatModifier stat =
    stat + 2


baseStatPointModifier : Int -> Int
baseStatPointModifier points =
    points + 3


incrementCharacterStat : CharacterStat -> Character -> Character
incrementCharacterStat characterStat (Character character) =
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
    Character { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints - 1, class = updatedClass }


incrementStat : StatHolder -> StatHolder
incrementStat statHolder =
    { statHolder | value = statHolder.value + 1 }


decrementCharacterStat : CharacterStat -> Character -> Character
decrementCharacterStat characterStat (Character character) =
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
    Character { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints + 1, class = updatedClass }


decrementStat : StatHolder -> StatHolder
decrementStat statHolder =
    { statHolder | value = statHolder.value-1 }


updateClass : Dict String StatHolder -> CharacterClass
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


getStatValue : Dict String StatHolder -> CharacterStat -> Int
getStatValue stats characterStat =
    let
        statEntry =
            Dict.get (characterStatToString characterStat) stats
    in
    case statEntry of
        Just statHolder ->
            statHolder.value

        Nothing ->
            0


updateCharacterStats : Character -> List Int -> Character
updateCharacterStats (Character character) randomList =
    let
        ( baseStatPoints, statList ) =
            splitRandomList randomList

        statPoints =
            baseStatPointModifier baseStatPoints
    in
    Character { character | stats = updateStats character.stats statList, statPoints = statPoints, experience = 1 }


splitRandomList : List Int -> ( Int, List Int )
splitRandomList randomList =
    case randomList of
        statPoints :: statList ->
            ( statPoints, statList )

        [] ->
            ( 0, [] )


updateStats : Dict String StatHolder -> List Int -> Dict String StatHolder
updateStats oldCharacterStats statList =
    let
        updater =
            setNewStatValue statList

        oldCharacterStatList =
            Dict.toList oldCharacterStats
    in
    List.indexedMap updater oldCharacterStatList
        |> Dict.fromList


setNewStatValue : List Int -> Int -> ( String, StatHolder ) -> ( String, StatHolder )
setNewStatValue statList index oldStat =
    let
        newArray =
            Array.fromList statList

        newValue =
            Maybe.withDefault 0 (Array.get index newArray)

        statString =
            Tuple.first oldStat

        statHolder =
            Tuple.second oldStat
    in
    ( statString, { statHolder | value = baseStatModifier newValue } )



-- Getters & Setters


getName : Character -> Maybe String
getName (Character character) =
    character.name


setName : Character -> String -> Character
setName (Character character) newName =
    Character { character | name = Just newName }


getClassString : Character -> String
getClassString (Character character) =
    classToString character.class


getStatPoints : Character -> Int
getStatPoints (Character character) =
    character.statPoints


getStatList : Character -> List Stat
getStatList (Character character) =
    Dict.toList character.stats
        |> List.sortWith (\a b -> compare (Tuple.second a).order (Tuple.second b).order)
        |> List.map getStatRecord


getStatRecord : ( String, StatHolder ) -> Stat
getStatRecord ( statName, statHolder ) =
    { name = statName, stat = statHolder.stat, value = statHolder.value }


getXP : Character -> Int
getXP (Character character) =
    character.experience