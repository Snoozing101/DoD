module Main exposing (main, buyItem, Model, init, Page(..))

import Browser
import Character exposing (Character)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Equipment exposing (Equipment, EquipmentCategory(..), equipmentCategorytoString, equipmentList)
import Random



---- MODEL ----


type alias Option =
    { page : Page
    , description : String
    }


options : List Option
options =
    [ { page = DungeonGenerator, description = "Dungeon Generator" }
    , { page = CharacterGenerator, description = "Character Generator" }
    , { page = TheGame, description = "Dungeon of Doom - the game" }
    ]


type Page
    = MenuPage
    | DungeonGenerator
    | CharacterGenerator
    | TheGame
    | ShopPage


type alias Model =
    { currPage : Page
    , character : Character
    , shop : List Equipment
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = MenuPage
      , character = Character.initStats Nothing
      , shop = Equipment.equipmentList
      }
    , Cmd.batch [ Random.generate NewCharacter newStats, Random.generate NewGold initialGold ]
    )



---- SUBSCRITPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type Msg
    = OptionSelected Page
    | NewCharacter (List Int)
    | NewGold Int
    | ReRollCharacter
    | IncrementStat Character.CharacterStat
    | DecrementStat Character.CharacterStat
    | UpdateName String
    | BuyItem Equipment.Item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionSelected page ->
            ( optionUpdate model page, Cmd.none )

        NewCharacter stats ->
            ( updateModelStats model stats, Cmd.none )

        NewGold gold ->
            ( updateModelGold model gold, Cmd.none )

        ReRollCharacter ->
            ( model, Cmd.batch [ Random.generate NewCharacter newStats, Random.generate NewGold initialGold ] )

        IncrementStat characterStat ->
            ( { model | character = Character.incrementCharacterStat characterStat model.character }, Cmd.none )

        DecrementStat characterStat ->
            ( { model | character = Character.decrementCharacterStat characterStat model.character }, Cmd.none )

        UpdateName newName ->
            ( { model | character = Character.setName model.character newName }, Cmd.none )

        BuyItem item ->
            ( buyItem model item, Cmd.none )


optionUpdate : Model -> Page -> Model
optionUpdate model selected =
    { model | currPage = selected }


updateModelStats : Model -> List Int -> Model
updateModelStats model statList =
    { model | character = Character.updateCharacterStats model.character statList }


updateModelGold : Model -> Int -> Model
updateModelGold model gold =
    { model | character = Character.initGold model.character gold }



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.currPage of
        MenuPage ->
            menuPage

        DungeonGenerator ->
            holdingPage "Dungeon Generator"

        CharacterGenerator ->
            characterGeneratorPage model.character

        TheGame ->
            holdingPage "The Game"

        ShopPage ->
            shopPage model


menuPage : Browser.Document Msg
menuPage =
    { title = "Dungeon of Doom"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                [ column
                    [ centerX
                    , Border.width 4
                    , Border.color white
                    ]
                    [ el
                        [ paddingXY 100 10
                        , Font.color green
                        , Font.size 40
                        ]
                      <|
                        text "Dungeon of Doom"
                    , column
                        [ width fill
                        , Font.color white
                        , paddingXY 40 10
                        , Border.widthEach
                            { top = 4
                            , left = 0
                            , right = 0
                            , bottom = 0
                            }
                        , Border.color white
                        , spacing 5
                        ]
                        (List.map buildList options)
                    ]
                ]
        ]
    }


holdingPage : String -> Browser.Document Msg
holdingPage pageName =
    { title = "Dungeon of Doom - " ++ pageName
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                [ el [ Font.size 40, Font.color white ] <|
                    text pageName
                , backButton
                ]
        ]
    }


shopPage : Model -> Browser.Document Msg
shopPage model =
    { title = "Dungeon of Doom - Shop"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                ((el [ Font.size 20, Font.color white ] <| text ("Gold Coins: " ++ String.fromInt (Character.getGold model.character)))
                    :: printArmouryItems model
                )
        ]
    }


printArmouryItems : Model -> List (Element Msg)
printArmouryItems model =
    let
        classEquipmentList =
            model.shop
                |> List.filter (\x -> List.member (Character.getClass model.character) x.usableBy)

        printClassShop =
            printShopCategory classEquipmentList
    in
    printClassShop Armoury
        ++ printClassShop Accoutrements
        ++ printClassShop Emporium


printShopCategory : List Equipment -> EquipmentCategory -> List (Element Msg)
printShopCategory classEquipmentList category =
    [ el [ Font.size 20, Font.color white ] <| text (equipmentCategorytoString category)
    , row [ paddingXY 50 10 ]
        [ column []
            (classEquipmentList
                |> List.filter (\x -> x.category == category)
                |> buildEquipmentTable
            )
        ]
    ]


buildEquipmentTable : List Equipment -> List (Element Msg)
buildEquipmentTable categoryList =
    [ table [ Font.size 20, Font.color white, spacingXY 20 0 ]
        { data = categoryList
        , columns =
            [ { header = el [ Font.alignLeft, Font.color green ] <| text "Item"
              , width = fill
              , view =
                    \item ->
                        if item.stockStatus == Equipment.OutOfStock then
                            el [ Font.alignLeft, Font.strike ] <| text (Equipment.itemToString item.item)
                        else
                            el [ Font.alignLeft ] <| text (Equipment.itemToString item.item)
              }
            , { header = el [ Font.color green ] <| text "Price"
              , width = fill
              , view =
                    \item ->
                        if item.stockStatus == Equipment.OutOfStock then
                            el [ Font.alignRight, Font.strike ] <| text (String.fromInt item.price)
                        else
                            el [ Font.alignRight ] <| text (String.fromInt item.price)
              }
            , { header = el [ Font.color green ] <| text "Qty"
              , width = fill
              , view =
                    \item ->
                        case item.stockStatus of
                            Equipment.InStock ->
                                el [ Font.alignRight ] <| text "1"
                                
                            Equipment.OutOfStock ->
                                el [ Font.alignRight ] <| text "0"

                            Equipment.InfiniteStock ->
                                el [ Font.alignRight ] <| text "∞"
              }
            , { header = none
              , width = fill
              , view =
                    \item ->
                        if item.stockStatus == Equipment.OutOfStock then
                            none
                        else
                            el [ Font.alignRight ] <|
                                Input.button
                                    [ Background.color blue
                                    , Element.focused
                                        [ Background.color blue ]
                                    ]
                                    { onPress = Just (BuyItem item.item)
                                    , label = text "Buy"
                                    }
              }
            ]
        }
    ]


characterGeneratorPage : Character -> Browser.Document Msg
characterGeneratorPage character =
    { title = "Dungeon of Doom - Character Generator"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                ([ Input.text [ Font.size 20, Font.color black, width (fill |> minimum 300 |> maximum 300) ]
                    { onChange = UpdateName
                    , text = Maybe.withDefault "" (Character.getName character)
                    , placeholder = Just (Input.placeholder [ Font.size 20, Font.color black ] <| text "Enter character name")
                    , label = Input.labelLeft [ Font.size 20, Font.color white ] <| text "Name: "
                    }
                 , el [ Font.size 20, Font.color white ] <| text (Character.getClassString character)
                 ]
                    ++ List.map (printStats (Character.getStatPoints character)) (Character.getStatList character)
                    ++ [ buildStatElement "Stat Points" (Character.getStatPoints character)
                       , buildStatElement "Experience" (Character.getXP character)
                       , buildStatElement "Gold" (Character.getGold character)
                       , backButton
                       , rerollButton
                       , shopButton
                       ]
                )
        ]
    }


printStats : Int -> Character.Stat -> Element Msg
printStats statPoints { name, stat, value } =
    row []
        [ buildStatElement name value
        , adjustButtons statPoints stat value
        ]


adjustButtons : Int -> Character.CharacterStat -> Int -> Element Msg
adjustButtons statPoints characterStat statValue =
    let
        incrementMessage =
            processIncrement statPoints characterStat

        decrementMessage =
            processDecrement statValue characterStat
    in
    row [ Font.color white, spacing 3, padding 5 ]
        [ Input.button
            [ width (px 20)
            , Border.width 1
            , Border.color darkGrey
            , Background.color grey
            , Font.color black
            ]
            { onPress = incrementMessage
            , label = text "+"
            }
        , Input.button
            [ width (px 20)
            , Border.width 1
            , Border.color darkGrey
            , Background.color grey
            , Font.color black
            ]
            { onPress = decrementMessage
            , label = text "-"
            }
        ]


processIncrement : Int -> Character.CharacterStat -> Maybe Msg
processIncrement statPoints characterStat =
    if statPoints > 0 then
        Just (IncrementStat characterStat)

    else
        Nothing


processDecrement : Int -> Character.CharacterStat -> Maybe Msg
processDecrement statValue characterStat =
    if statValue > 0 then
        Just (DecrementStat characterStat)

    else
        Nothing


buildStatElement : String -> Int -> Element msg
buildStatElement name val =
    el [ Font.size 20, Font.color white ] <|
        text (name ++ ": " ++ String.fromInt val)


rerollButton : Element Msg
rerollButton =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just ReRollCharacter
        , label = text "Reroll Character"
        }


backButton : Element Msg
backButton =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just (OptionSelected MenuPage)
        , label = text "Main Menu"
        }


shopButton : Element Msg
shopButton =
    Input.button
        [ Background.color blue
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just (OptionSelected ShopPage)
        , label = text "Shop"
        }


buildList : Option -> Element Msg
buildList option =
    el
        [ mouseOver [ Background.color green, Font.color blue ]
        , Element.Events.onClick (OptionSelected option.page)
        , padding 2
        , Font.alignLeft
        , width fill
        ]
    <|
        text option.description


blue : Color
blue =
    rgb255 0 0 255


black : Color
black =
    rgb255 0 0 0


green : Color
green =
    rgb255 0 255 0


white : Color
white =
    rgb255 255 255 255


grey : Color
grey =
    rgb255 200 200 200


darkGrey : Color
darkGrey =
    rgb255 100 100 100



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


newStats : Random.Generator (List Int)
newStats =
    Random.list 9 (Random.int 1 5)


initialGold : Random.Generator Int
initialGold =
    Random.int 1 60


buyItem : Model -> Equipment.Item -> Model
buyItem model item =
    let
        itemCost =
            Equipment.getPrice model.shop item

        newGold =
            Character.getGold model.character - itemCost

        oldInventory =
            Character.getInventory model.character

        stockStatus =
            item
                |> Equipment.getStockStatus model.shop

        newCharacter =
            model.character
                |> Character.setGold newGold
                |> Character.addToInventory item
    in
    if (newGold < 0) || (stockStatus == Equipment.OutOfStock && List.member item oldInventory) then
        model

    else if stockStatus == Equipment.InStock then
        { model | character = newCharacter, shop = Equipment.setOutOfStock model.shop item }

    else
        { model | character = newCharacter }
