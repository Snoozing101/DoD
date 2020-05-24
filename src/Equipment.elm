module Equipment exposing
    ( Equipment
    , EquipmentCategory(..)
    , Item(..)
    , equipmentCategorytoString
    , equipmentList
    , getPrice
    , isUnique
    , itemToString
    )

import Class exposing (CharacterClass(..))


type Item
    = TwoHandedSword
    | BroardSword
    | ShortSword
    | Axe
    | Mace
    | Flail
    | Dagger
    | Gauntlet
    | HeavyArmour
    | ChainArmour
    | LeatherArmour
    | HeavyRobe
    | GoldHelmet
    | Headpiece
    | Shield
    | Torch
    | Necronomicon
    | Scrolls
    | Ring
    | MysticAmulet
    | Sash
    | Cloak
    | HealingSalve
    | Potions


itemToString : Item -> String
itemToString item =
    case item of
        TwoHandedSword ->
            "2 Handed Sword"

        BroardSword ->
            "Broardsword"

        ShortSword ->
            "Short Sword"

        Axe ->
            "Axe"

        Mace ->
            "Mace"

        Flail ->
            "Flail"

        Dagger ->
            "Dagger"

        Gauntlet ->
            "Gauntlet"

        HeavyArmour ->
            "Heavy Armour"

        ChainArmour ->
            "Chain Armour"

        LeatherArmour ->
            "Leather Armour"

        HeavyRobe ->
            "Heavy Robe"

        GoldHelmet ->
            "Gold Helmet"

        Headpiece ->
            "Headpiece"

        Shield ->
            "Shield"

        Torch ->
            "Torch"

        Necronomicon ->
            "Necronomicon"

        Scrolls ->
            "Scrolls"

        Ring ->
            "Ring"

        MysticAmulet ->
            "Mystic Amulet"

        Sash ->
            "Sash"

        Cloak ->
            "Cloak"

        HealingSalve ->
            "Healing Salve"

        Potions ->
            "Potions"


type alias Equipment =
    { usableBy : List CharacterClass
    , price : Int
    , power : Int
    , item : Item
    , category : EquipmentCategory
    , unique : Bool
    }


type EquipmentCategory
    = Armoury
    | Accoutrements
    | Emporium


equipmentCategorytoString : EquipmentCategory -> String
equipmentCategorytoString category =
    case category of
        Armoury ->
            "Armoury"

        Accoutrements ->
            "Accoutrements"

        Emporium ->
            "Emporium"


equipmentList : List Equipment
equipmentList =
    [ Equipment [ Barbarian ] 20 5 TwoHandedSword Armoury True
    , Equipment [ Warrior, Barbarian ] 16 4 BroardSword Armoury True
    , Equipment [ Wanderer, Warrior, Barbarian ] 12 3 ShortSword Armoury True
    , Equipment [ Wanderer, Warrior, Barbarian ] 15 3 Axe Armoury True
    , Equipment [ Wanderer, Warrior, Barbarian ] 8 2 Mace Armoury True
    , Equipment [ Warrior, Barbarian ] 10 2 Flail Armoury True
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 8 1 Dagger Armoury True
    , Equipment [ Wanderer, Warrior, Barbarian ] 6 1 Gauntlet Armoury True
    , Equipment [ Warrior, Barbarian ] 18 5 HeavyArmour Accoutrements True
    , Equipment [ Warrior, Barbarian ] 15 4 ChainArmour Accoutrements True
    , Equipment [ Wanderer, Warrior, Barbarian ] 9 3 LeatherArmour Accoutrements True
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 9 1 HeavyRobe Accoutrements True
    , Equipment [ Warrior, Barbarian ] 14 2 GoldHelmet Accoutrements True
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 8 1 Headpiece Accoutrements True
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 6 3 Shield Accoutrements True
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 Torch Accoutrements True
    , Equipment [ Wanderer, Cleric, Magician ] 20 4 Necronomicon Emporium True
    , Equipment [ Magician ] 15 3 Scrolls Emporium True
    , Equipment [ Wanderer, Cleric, Magician ] 14 2 Ring Emporium True
    , Equipment [ Wanderer, Magician ] 12 2 MysticAmulet Emporium True
    , Equipment [ Wanderer, Cleric, Magician ] 10 3 Sash Emporium True
    , Equipment [ Wanderer, Cleric, Magician ] 8 1 Cloak Emporium True
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 HealingSalve Emporium False
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 Potions Emporium False
    ]


findEquipment : Item -> Maybe Equipment
findEquipment item =
    List.filter (\listItem -> listItem.item == item) equipmentList
        |> List.head


getPrice : Item -> Int
getPrice item =
    item
        |> findEquipment
        |> getPriceFromList


getPriceFromList : Maybe Equipment -> Int
getPriceFromList maybeEquipment =
    case maybeEquipment of
        Just equipment ->
            equipment.price

        Nothing ->
            0


isUnique : Item -> Bool
isUnique item =
    let
        foundEquipment =
            findEquipment item
    in
    case foundEquipment of
        Just equipment ->
            equipment.unique

        Nothing ->
            True
