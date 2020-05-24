module Equipment exposing (Equipment, EquipmentCategory(..), equipmentCategorytoString, equipmentList, itemToString)

import Class exposing (CharacterClass(..))

type Item =
    TwoHandedSword
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
    }

type EquipmentCategory =
    Armoury
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
    [ Equipment [ Barbarian ] 20 5 TwoHandedSword Armoury
    , Equipment [ Warrior, Barbarian ] 16 4 BroardSword Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 12 3 ShortSword Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 15 3 Axe Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 8  2 Mace Armoury
    , Equipment [ Warrior, Barbarian ] 10 2 Flail Armoury
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 8  1 Dagger Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 6  1 Gauntlet Armoury
    , Equipment [ Warrior, Barbarian ] 18 5 HeavyArmour Accoutrements
    , Equipment [ Warrior, Barbarian ] 15 4 ChainArmour Accoutrements
    , Equipment [ Wanderer, Warrior, Barbarian ] 9  3 LeatherArmour Accoutrements
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 9  1 HeavyRobe Accoutrements
    , Equipment [ Warrior, Barbarian ] 14 2 GoldHelmet Accoutrements
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 8  1 Headpiece Accoutrements
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 6  3 Shield Accoutrements
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 Torch Accoutrements
    , Equipment [ Wanderer, Cleric, Magician ] 20 4 Necronomicon Emporium
    , Equipment [ Magician ] 15 3 Scrolls Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 14 2 Ring Emporium
    , Equipment [ Wanderer, Magician ] 12 2 MysticAmulet Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 10 3 Sash Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 8  1 Cloak Emporium
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 HealingSalve Emporium
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 Potions Emporium
    ]                                                                   
