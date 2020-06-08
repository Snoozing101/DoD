module Character exposing
    ( Character
    , CharacterStat(..)
    , Stat
    , addToInventory
    , decrementCharacterStat
    , encodeCharacter
    , getClass
    , getClassString
    , getGold
    , getInventory
    , getName
    , getStatList
    , getStatPoints
    , getXP
    , incrementCharacterStat
    , initGold
    , initStats
    , setGold
    , setName
    , updateCharacterStats
    )

import Array
import Class exposing (CharacterClass(..), classToString)
import Dict exposing (Dict)
import Equipment
import Json.Encode exposing (object, string)


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


type Character
    = Character
        { class : CharacterClass
        , name : Maybe String
        , stats : Dict String StatHolder
        , experience : Int
        , statPoints : Int
        , gold : Int
        , inventory : List Equipment.Item
        }


initStats : Maybe String -> Character
initStats characterName =
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
        , gold = 0
        , inventory = []
        }


makeStatDictEntry : CharacterStat -> Int -> Int -> ( String, StatHolder )
makeStatDictEntry stat value order =
    let
        statString =
            characterStatToString stat
    in
    ( statString, { stat = stat, value = value, order = order } )


baseStatModifier : Int -> Int
baseStatModifier stat =
    stat + 2


baseStatPointModifier : Int -> Int
baseStatPointModifier points =
    points + 3


baseGoldModifier : Int -> Int
baseGoldModifier initialGold =
    initialGold + 120


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
    { statHolder | value = statHolder.value - 1 }


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
        Magician

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


initGold : Character -> Int -> Character
initGold (Character character) newGold =
    let
        initalGold =
            baseGoldModifier newGold
    in
    Character { character | gold = initalGold }


setGold : Int -> Character -> Character
setGold newGold (Character character) =
    Character { character | gold = newGold }


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


getClass : Character -> CharacterClass
getClass (Character character) =
    character.class


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


getGold : Character -> Int
getGold (Character character) =
    character.gold


getInventory : Character -> List Equipment.Item
getInventory (Character character) =
    character.inventory


addToInventory : Equipment.Item -> Character -> Character
addToInventory item (Character character) =
    Character { character | inventory = character.inventory ++ [ item ] }


encodeCharacter : Character -> Json.Encode.Value
encodeCharacter (Character character) =
    object
        [ ( "Class", string (classToString character.class) )
        , ( "Name", string (Maybe.withDefault "unknown" character.name) )
        , ( "Experience", Json.Encode.int character.experience )
        , ( "StatPoints", Json.Encode.int character.statPoints )
        , ( "Gold", Json.Encode.int character.gold )
        , ( "Stats", encodeStats character.stats )
        , ( "Inventory", Json.Encode.list string (encodeInventory character.inventory) )
        ]


encodeInventory : List Equipment.Item -> List String
encodeInventory inventory =
    inventory
    |> List.map Equipment.itemToString


encodeStats : Dict String StatHolder -> Json.Encode.Value
encodeStats stats =
    stats
        |> Dict.toList
        |> List.map statEntryToJson
        |> Json.Encode.object


statEntryToJson : ( String, StatHolder ) -> ( String, Json.Encode.Value )
statEntryToJson (key, statHolder) =
    ( key, Json.Encode.int statHolder.value )
