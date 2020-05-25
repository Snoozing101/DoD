module MainTests exposing (..)

import Character
import CharacterTests
import Class exposing (CharacterClass(..))
import Equipment exposing (Item(..))
import Expect
import Main exposing (buyItem)
import Test exposing (..)


all : Test
all =
    describe "Test main functions"
        [ test "Buy item adds to inventory, reduces gold" <|
            \_ ->
                let
                    newModel =
                        Main.Model Main.MenuPage (Character.initGold CharacterTests.buildBaseCharacter 0) Equipment.equipmentList

                    boughtItem =
                        buyItem newModel ShortSword

                    expectedInventory =
                        [ ShortSword ]

                    actualInventory =
                        Character.getInventory boughtItem.character

                    expectedGold =
                        108

                    actualGold =
                        Character.getGold boughtItem.character
                in
                if expectedInventory == actualInventory && expectedGold == actualGold then
                    Expect.pass

                else
                    Expect.fail "Didn't buy item"
        , test "Can't buy an item if insuffient funds" <|
            \_ ->
                let
                    newModel =
                        Main.Model Main.MenuPage (Character.initGold CharacterTests.buildBaseCharacter 0) Equipment.equipmentList

                    boughtItems =
                        List.range 1 21
                            |> List.foldl (\_ acc -> buyItem acc Potions) newModel

                    expectedInventorySize =
                        20

                    actualInventorySize =
                        List.length (Character.getInventory boughtItems.character)

                    expectedGold =
                        0

                    actualGold =
                        Character.getGold boughtItems.character
                in
                if expectedInventorySize == actualInventorySize && expectedGold == actualGold then
                    Expect.pass

                else
                    Expect.fail "Bought more than expected items"
        , test "Can't buy more than one of an item other that doesn't have InfiniteStock" <|
            \_ ->
                let
                    newModel =
                        Main.Model Main.MenuPage (Character.initGold CharacterTests.buildBaseCharacter 0) Equipment.equipmentList

                    boughtItems =
                        List.range 1 11
                            |> List.foldl (\_ acc -> buyItem acc ShortSword) newModel

                    expectedInventory =
                        [ ShortSword ]

                    actualInventory =
                        Character.getInventory boughtItems.character

                    expectedGold =
                        108

                    actualGold =
                        Character.getGold boughtItems.character
                in
                if expectedInventory == actualInventory && expectedGold == actualGold then
                    Expect.pass

                else
                    Expect.fail "Managed to buy more than one item that isn't InfiniteStock"
        , test "Status of InStock item goes OutOfStock once purchased" <|
            \_ ->
                let
                    newModel =
                        Main.Model Main.MenuPage (Character.initGold CharacterTests.buildBaseCharacter 0) Equipment.equipmentList

                    boughtItem =
                        buyItem newModel ShortSword

                    newStockStatus =
                        Equipment.getStockStatus boughtItem.shop ShortSword

                in
                Expect.equal newStockStatus Equipment.OutOfStock
        , test "Status of InfiniteStock item does not change when purchased" <|
            \_ ->
                let
                    newModel =
                        Main.Model Main.MenuPage (Character.initGold CharacterTests.buildBaseCharacter 0) Equipment.equipmentList

                    boughtItem =
                        buyItem newModel Potions

                    newStockStatus =
                        Equipment.getStockStatus boughtItem.shop Potions

                in
                Expect.equal newStockStatus Equipment.InfiniteStock
        ]
