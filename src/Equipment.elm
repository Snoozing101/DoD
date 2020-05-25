module Equipment exposing
    ( Equipment
    , EquipmentCategory(..)
    , Item(..)
    , StockStatus(..)
    , equipmentCategorytoString
    , equipmentList
    , getPrice
    , getStockStatus
    , itemToString
    , setOutOfStock
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


type StockStatus
    = InStock
    | OutOfStock
    | InfiniteStock


type alias Equipment =
    { usableBy : List CharacterClass
    , price : Int
    , power : Int
    , item : Item
    , category : EquipmentCategory
    , stockStatus : StockStatus
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
    [ Equipment [ Barbarian ] 20 5 TwoHandedSword Armoury InStock
    , Equipment [ Warrior, Barbarian ] 16 4 BroardSword Armoury InStock
    , Equipment [ Wanderer, Warrior, Barbarian ] 12 3 ShortSword Armoury InStock
    , Equipment [ Wanderer, Warrior, Barbarian ] 15 3 Axe Armoury InStock
    , Equipment [ Wanderer, Warrior, Barbarian ] 8 2 Mace Armoury InStock
    , Equipment [ Warrior, Barbarian ] 10 2 Flail Armoury InStock
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 8 1 Dagger Armoury InStock
    , Equipment [ Wanderer, Warrior, Barbarian ] 6 1 Gauntlet Armoury InStock
    , Equipment [ Warrior, Barbarian ] 18 5 HeavyArmour Accoutrements InStock
    , Equipment [ Warrior, Barbarian ] 15 4 ChainArmour Accoutrements InStock
    , Equipment [ Wanderer, Warrior, Barbarian ] 9 3 LeatherArmour Accoutrements InStock
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 9 1 HeavyRobe Accoutrements InStock
    , Equipment [ Warrior, Barbarian ] 14 2 GoldHelmet Accoutrements InStock
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 8 1 Headpiece Accoutrements InStock
    , Equipment [ Wanderer, Cleric, Warrior, Barbarian ] 6 3 Shield Accoutrements InStock
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 Torch Accoutrements InStock
    , Equipment [ Wanderer, Cleric, Magician ] 20 4 Necronomicon Emporium InStock
    , Equipment [ Magician ] 15 3 Scrolls Emporium InStock
    , Equipment [ Wanderer, Cleric, Magician ] 14 2 Ring Emporium InStock
    , Equipment [ Wanderer, Magician ] 12 2 MysticAmulet Emporium InStock
    , Equipment [ Wanderer, Cleric, Magician ] 10 3 Sash Emporium InStock
    , Equipment [ Wanderer, Cleric, Magician ] 8 1 Cloak Emporium InStock
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 HealingSalve Emporium InfiniteStock
    , Equipment [ Wanderer, Cleric, Magician, Warrior, Barbarian ] 6 1 Potions Emporium InfiniteStock
    ]


findEquipment : List Equipment -> Item -> Maybe Equipment
findEquipment list item =
    List.filter (\listItem -> listItem.item == item) list
        |> List.head


getPrice : List Equipment -> Item -> Int
getPrice list item =
    item
        |> findEquipment list
        |> getPriceFromList


getPriceFromList : Maybe Equipment -> Int
getPriceFromList maybeEquipment =
    case maybeEquipment of
        Just equipment ->
            equipment.price

        Nothing ->
            0


getStockStatus : List Equipment -> Item -> StockStatus
getStockStatus list item =
    item
        |> findEquipment list
        |> getStockStatusFromList


getStockStatusFromList : Maybe Equipment -> StockStatus
getStockStatusFromList maybeEquipment =
    case maybeEquipment of
        Just equipment ->
            equipment.stockStatus

        Nothing ->
            OutOfStock

setOutOfStock : List Equipment -> Item -> List Equipment
setOutOfStock list item =
    list
    |> List.map (\x -> setOutOfStockItem item x)
    

setOutOfStockItem : Item -> Equipment -> Equipment
setOutOfStockItem item equipment =
    if equipment.item == item && equipment.stockStatus == InStock then
        { equipment | stockStatus = OutOfStock }
    else
        equipment
    
