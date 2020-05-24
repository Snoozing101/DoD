module Equipment exposing (Equipment, EquipmentCategory(..), equipmentCategorytoSting, equipmentList)

import Class exposing (CharacterClass(..))

type alias Equipment =
    { usableBy : List CharacterClass
    , price : Int
    , power : Int
    , description : String
    , category : EquipmentCategory
    }

type EquipmentCategory =
    Armoury
    | Accoutrements
    | Emporium

equipmentCategorytoSting : EquipmentCategory -> String
equipmentCategorytoSting category =
    case category of
        Armoury ->
            "Armoury"

        Accoutrements ->
            "Accoutrements"

        Emporium ->
            "Emporium"

equipmentList : List Equipment
equipmentList =
    [ Equipment [ Barbarian ] 20 5 "2 Handed Sword" Armoury
    , Equipment [ Warrior, Barbarian ] 16 4 "Broardsword" Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 12 3 "Short Sword" Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 15 3 "Axe" Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 8  2 "Mace" Armoury
    , Equipment [ Warrior, Barbarian ] 10 2 "Flail" Armoury
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 8  1 "Dagger" Armoury
    , Equipment [ Wanderer, Warrior, Barbarian ] 6  1 "Gauntlet" Armoury
    , Equipment [ Warrior, Barbarian ] 18 5 "Heavy Armour" Accoutrements
    , Equipment [ Warrior, Barbarian ] 15 4 "Chain Armour" Accoutrements
    , Equipment [ Wanderer, Warrior, Barbarian ] 9  3 "Leather Armour" Accoutrements
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 9  1 "Heavy Robe" Accoutrements
    , Equipment [ Warrior, Barbarian ] 14 2 "Gold Helmet" Accoutrements
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 8  1 "Headpiece" Accoutrements
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 6  3 "Shield" Accoutrements
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 "Torch" Accoutrements
    , Equipment [ Wanderer, Cleric, Magician ] 20 4 "Necronomicon" Emporium
    , Equipment [ Magician ] 15 3 "Scrolls" Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 14 2 "Ring" Emporium
    , Equipment [ Wanderer, Magician ] 12 2 "Mystic Amulet" Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 10 3 "Sash" Emporium
    , Equipment [ Wanderer, Cleric, Magician ] 8  1 "Cloak" Emporium
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 "Healing Salve" Emporium
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6  1 "Potions" Emporium
    ]                                                                   
