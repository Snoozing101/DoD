module Tests exposing (..)

import Character exposing (CharacterClass(..), CharacterStat(..))
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
                    newStats =
                        Maybe.withDefault [] (List.tail getRandomValues)

                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getRandomValues

                    updatedStats =
                        Character.getStatList newCharacter
                            |> List.map (\statRecord -> statRecord.value)
                in
                Expect.equal (List.map (\x -> x + 2) newStats) updatedStats
        , test "Are stat points set correctly" <|
            \_ ->
                let
                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getRandomValues
                in
                Expect.equal (Character.getStatPoints newCharacter) 6
        , test "Decrement stats correctly" <|
            \_ ->
                let
                    matchValues =
                        getRandomValues
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.map (\x -> x + 1)

                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getRandomValues

                    decrementStats =
                        Character.getStatList newCharacter
                            |> List.map (\statRecord -> statRecord.stat)
                            |> List.foldl (\stat char -> Character.decrementCharacterStat stat char) newCharacter

                    updatedStats =
                        Character.getStatList decrementStats
                            |> List.map (\statRecord -> statRecord.value)
                in
                Expect.equal updatedStats matchValues
        , test "Increment stats correctly" <|
            \_ ->
                let
                    matchValues =
                        getRandomValues
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.map (\x -> x + 3)

                    newCharacter =
                        Character.updateCharacterStats buildBaseCharacter getRandomValues

                    incrementStats =
                        Character.getStatList newCharacter
                            |> List.map (\statRecord -> statRecord.stat)
                            |> List.foldl (\stat char -> Character.incrementCharacterStat stat char) newCharacter

                    updatedStats =
                        Character.getStatList incrementStats
                            |> List.map (\statRecord -> statRecord.value)
                in
                Expect.equal updatedStats matchValues
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
        , test "Test Mage rules" <|
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
                Expect.equal (Character.getClassString mageCharacter) "Mage"
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
        ]



-- Helper functions


getFixedValues : List Int
getFixedValues =
    [ 3, 4, 4, 4, 4, 4, 4, 4 ]


getRandomValues : List Int
getRandomValues =
    [ 3, 6, 2, 5, 1, 8, 2, 5 ]


getTestName : String
getTestName =
    "Frodo"


buildBaseCharacter : Character.Character
buildBaseCharacter =
    Character.init (Just getTestName)
