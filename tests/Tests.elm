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
                let
                    testName =
                        "Frodo"

                    character =
                        Character.init (Just testName)
                in
                Expect.equal (Character.getName character) (Just testName)
        , test "Initial class should be Wanderer" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")
                in
                Expect.equal (Character.getClassString character) "Wanderer"
        , test "There should be 7 stats" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")

                    statList =
                        Character.getStatList character
                in
                Expect.equal (List.length statList) 7
        , test "Can update the stat values" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 3, 6, 2, 5, 1, 8, 2, 5 ]

                    newStats =
                        Maybe.withDefault [] (List.tail randomValues)

                    newCharacter =
                        Character.updateCharacterStats character randomValues

                    updatedStats =
                        Character.getStatList newCharacter
                            |> List.map (\statRecord -> statRecord.value)
                in
                Expect.equal (List.map (\x -> x + 2) newStats) updatedStats
        , test "Are stat points set correctly" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 3, 6, 2, 5, 1, 8, 2, 5 ]

                    newCharacter =
                        Character.updateCharacterStats character randomValues
                in
                Expect.equal (Character.getStatPoints newCharacter) 6
        , test "Decrement stats correctly" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 3, 6, 2, 5, 1, 8, 2, 5 ]

                    matchValues =
                        randomValues
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.map (\x -> x + 1)

                    newCharacter =
                        Character.updateCharacterStats character randomValues

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
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 3, 6, 2, 5, 1, 8, 2, 5 ]

                    matchValues =
                        randomValues
                            |> List.tail
                            |> Maybe.withDefault []
                            |> List.map (\x -> x + 3)

                    newCharacter =
                        Character.updateCharacterStats character randomValues

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
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 3, 6, 6, 6, 6, 6, 6, 6 ]

                    newCharacter =
                        Character.updateCharacterStats character randomValues

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
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 2, 4, 4, 4, 4, 4, 4, 4 ]

                    newCharacter =
                        Character.updateCharacterStats character randomValues

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
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 2, 4, 4, 4, 4, 4, 4, 4 ]

                    newCharacter =
                        Character.updateCharacterStats character randomValues

                    warriorCharacter =
                        newCharacter
                            |> Character.incrementCharacterStat Strength
                            |> Character.incrementCharacterStat Strength
                in
                Expect.equal (Character.getClassString warriorCharacter) "Warrior"
        , test "Test Barbarian rules" <|
            \_ ->
                let
                    character =
                        Character.init (Just "Frodo")

                    randomValues =
                        [ 2, 4, 4, 4, 4, 4, 4, 4 ]

                    newCharacter =
                        Character.updateCharacterStats character randomValues

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
