module CharacterTests exposing (..)

import Character exposing (CharacterStat(..))
import Class exposing (CharacterClass(..))
import Equipment exposing (Item(..))
import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Test character functions"
        [ test "Create a character with a known name" <|
            \_ ->
                Expect.equal (Character.getName buildBaseCharacter) (Just getTestName)
        , test "Initial class should be Wanderer" <|
            \_ ->
                Expect.equal (Character.getClassString buildBaseCharacter) "Wanderer"
        , test "There should be 7 stats" <|
            \_ ->
                let
                    statList =
                        Character.getStatList buildBaseCharacter
                in
                Expect.equal (List.length statList) 7
        , test "Can update the stat values" <|
            \_ ->
                let
                    expectedStats =
                        getAscendingValues
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.map (\x -> x + 2)

                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getAscendingValues

                    updatedStats =
                        Character.getStatList newCharacter
                            |> List.map (\statRecord -> statRecord.value)
                            |> List.sort
                in
                Expect.equal expectedStats updatedStats
        , test "Are stat points set correctly" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getRandomValues
                in
                Expect.equal (Character.getStatPoints newCharacter) 6
        , test "Is inital gold set correctly" <|
            \_ ->
                let
                    newCharacter =
                        Character.initGold buildBaseCharacter 50
                in
                Expect.equal (Character.getGold newCharacter) 170
        , test "Decrement stats correctly" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    decrementStats =
                        Character.decrementCharacterStat Strength newCharacter

                    defaultStat =
                        Character.Stat "Strength" Strength 0

                    updatedStats =
                        Character.getStatList decrementStats
                            |> List.filter (\x -> x.stat == Strength)
                            |> List.head
                            |> Maybe.withDefault defaultStat

                    matchValue =
                        Character.Stat "Strength" Strength 5
                in
                Expect.equal updatedStats matchValue
        , test "Increment stats correctly" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    incrementStats =
                        Character.incrementCharacterStat Strength newCharacter

                    defaultStat =
                        Character.Stat "Strength" Strength 0

                    updatedStats =
                        Character.getStatList incrementStats
                            |> List.filter (\x -> x.stat == Strength)
                            |> List.head
                            |> Maybe.withDefault defaultStat

                    matchValue =
                        Character.Stat "Strength" Strength 7
                in
                Expect.equal updatedStats matchValue
        , test "Test Cleric rules" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    clericCharacter =
                        newCharacter
                            |> Character.incrementCharacterStat Intelligence
                            |> Character.incrementCharacterStat Morality
                            |> Character.incrementCharacterStat Morality
                in
                Expect.equal (Character.getClassString clericCharacter) "Cleric"
        , test "Test Magician rules" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    mageCharacter =
                        newCharacter
                            |> Character.incrementCharacterStat Intelligence
                            |> Character.incrementCharacterStat Intelligence
                            |> Character.incrementCharacterStat Intelligence
                            |> Character.incrementCharacterStat Aura
                            |> Character.incrementCharacterStat Aura
                in
                Expect.equal (Character.getClassString mageCharacter) "Magician"
        , test "Test Warrior rules" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    warriorCharacter =
                        newCharacter
                            |> Character.incrementCharacterStat Strength
                            |> Character.incrementCharacterStat Strength
                in
                Expect.equal (Character.getClassString warriorCharacter) "Warrior"
        , test "Test Barbarian rules" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    barbarianCharacter =
                        newCharacter
                            |> Character.decrementCharacterStat Morality
                            |> Character.incrementCharacterStat Strength
                            |> Character.incrementCharacterStat Strength
                            |> Character.incrementCharacterStat Strength
                            |> Character.incrementCharacterStat Agility
                in
                Expect.equal (Character.getClassString barbarianCharacter) "Barbarian"
        , test "Stat order returned matches original" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getFixedValues

                    statStrings =
                        Character.getStatList newCharacter
                            |> List.map (\x -> x.name)

                    expectedOrder =
                        [ "Strength", "Vitality", "Agility", "Intelligence", "Luck", "Aura", "Morality" ]
                in
                Expect.equal statStrings expectedOrder
        ]



-- Helper functions


getFixedValues : List Int
getFixedValues =
    [ 3, 4, 4, 4, 4, 4, 4, 4 ]


getRandomValues : List Int
getRandomValues =
    [ 3, 6, 2, 5, 1, 8, 2, 5 ]


getAscendingValues : List Int
getAscendingValues =
    [ 0, 1, 2, 3, 4, 5, 6, 7 ]


getTestName : String
getTestName =
    "Frodo"


buildBaseCharacter : Character.Character
buildBaseCharacter =
    Character.initStats (Just getTestName)
