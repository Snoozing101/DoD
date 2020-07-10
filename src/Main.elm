port module Main exposing (Model, OfferMessage(..), Page(..), buyItem, checkOffer, init, main)

import Browser
import Character exposing (Character)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Equipment exposing (Equipment, EquipmentCategory(..), equipmentCategorytoString, equipmentList)
import Json.Encode
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
    | SaveCharacter


type alias Model =
    { currPage : Page
    , makeOffer : Bool
    , itemUnderOffer : Maybe Equipment.Item
    , currentOffer : String
    , secretPrice : Int
    , offerMessage : OfferMessage
    , character : Character
    , shop : List Equipment
    , dropdownState : Dropdown.State String
    , optionPicked : Maybe String
    , characterList : List String
    , tempCharacter : String
    }


type OfferMessage
    = NoOffer
    | RejectedOfferMessage
    | AcceptedOfferMessage


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = MenuPage
      , makeOffer = False
      , itemUnderOffer = Nothing
      , currentOffer = ""
      , secretPrice = 0
      , offerMessage = NoOffer
      , character = Character.initStats Nothing
      , shop = Equipment.equipmentList
      , dropdownState = Dropdown.init "dropdown"
      , optionPicked = Nothing
      , characterList = []
      , tempCharacter = ""
      }
    , Cmd.none
    )



---- SUBSCRITPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ characterListReceiver CharListRecv
        , characterReceiver CharRecv
        ]



---- U PDATE ----


type Msg
    = OptionSelected Page
    | NewCharacter (List Int)
    | NewGold Int
    | IncrementStat Character.CharacterStat
    | DecrementStat Character.CharacterStat
    | UpdateName String
    | BuyItem Equipment.Item
    | BargainForItem Equipment.Item
    | UpdateOffer String
    | CancelOffer
    | MakeOffer
    | NewBargainPrice Int
    | SaveCharacterToDB
    | OptionPicked (Maybe String)
    | DropdownMsg (Dropdown.Msg String)
    | CharListRecv (List String)
    | CharRecv String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionSelected page ->
            ( optionUpdate model page, processPageCmd page )

        NewCharacter stats ->
            ( updateModelStats model stats, Cmd.none )

        NewGold gold ->
            ( updateModelGold model gold, Cmd.none )

        IncrementStat characterStat ->
            ( { model | character = Character.incrementCharacterStat characterStat model.character }, Cmd.none )

        DecrementStat characterStat ->
            ( { model | character = Character.decrementCharacterStat characterStat model.character }, Cmd.none )

        UpdateName newName ->
            ( { model | character = Character.setName model.character newName }, Cmd.none )

        BuyItem item ->
            ( buyItem model item, Cmd.none )

        BargainForItem item ->
            ( { model | makeOffer = True, itemUnderOffer = Just item, currentOffer = "", offerMessage = NoOffer }, Random.generate NewBargainPrice bargainPrice )

        UpdateOffer offer ->
            ( { model | currentOffer = offer, offerMessage = NoOffer }, Cmd.none )

        CancelOffer ->
            ( { model | makeOffer = False, itemUnderOffer = Nothing }, Cmd.none )

        MakeOffer ->
            ( processOffer model, Cmd.none )

        NewBargainPrice priceDifference ->
            ( { model | secretPrice = getSecretPrice model priceDifference }, Cmd.none )

        SaveCharacterToDB ->
            ( { model | currPage = MenuPage }, saveCharacterToDB (Json.Encode.encode 2 (Character.encodeCharacter model.character)) )

        OptionPicked option ->
            ( { model | optionPicked = option }, retrieveCharacter (processSelectedCharacter option) )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState model.characterList
            in
            ( { model | dropdownState = state }, cmd )

        CharListRecv newCharacterList ->
            ( { model | characterList = newCharacterList }, Cmd.none )

        CharRecv retrievedChar ->
            ( { model | tempCharacter = retrievedChar }, Cmd.none )


processSelectedCharacter : Maybe String -> String
processSelectedCharacter selectedOption =
    case selectedOption of
        Just character ->
            character
            |> String.words
            |> List.head
            |> Maybe.withDefault "" 

        Nothing ->
            ""


processPageCmd : Page -> Cmd Msg
processPageCmd page =
    if page == CharacterGenerator then
        Cmd.batch [ Random.generate NewCharacter newStats, Random.generate NewGold initialGold ]

    else if page == TheGame then
        retrieveCharacterList ()

    else
        Cmd.none


processOffer : Model -> Model
processOffer model =
    model
        |> checkOffer
        |> updateOfferResult model


updateOfferResult : Model -> OfferMessage -> Model
updateOfferResult model offerMessage =
    let
        newModel =
            { model | offerMessage = offerMessage }
    in
    if offerMessage == AcceptedOfferMessage then
        case model.itemUnderOffer of
            Just item ->
                buyItem newModel item

            Nothing ->
                newModel

    else
        newModel


checkOffer : Model -> OfferMessage
checkOffer model =
    let
        offerValue =
            Maybe.withDefault 0 (String.toInt model.currentOffer)
    in
    if offerValue >= model.secretPrice then
        AcceptedOfferMessage

    else
        RejectedOfferMessage


getSecretPrice : Model -> Int -> Int
getSecretPrice model priceDifference =
    case model.itemUnderOffer of
        Just item ->
            Equipment.getPrice model.shop item - priceDifference

        Nothing ->
            0


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
            theGamePage model

        ShopPage ->
            shopPage model

        SaveCharacter ->
            saveCharacterScreen model.character



---- VIEW MENU ----


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



---- VIEW OFFER DIALOG ----


offerDialog : Model -> Element Msg
offerDialog model =
    let
        itemString =
            case model.itemUnderOffer of
                Just item ->
                    Equipment.itemToString item

                Nothing ->
                    "Unknown"

        messageText =
            case model.offerMessage of
                NoOffer ->
                    ""

                RejectedOfferMessage ->
                    "Offer Rejected"

                AcceptedOfferMessage ->
                    "Tis Yours!"
    in
    if model.makeOffer then
        el
            [ Background.color black
            , Font.color white
            , Font.size 20
            , Font.alignLeft
            , padding 10
            , width (px 350)
            , height (px 180)
            , centerX
            , centerY
            , Border.rounded 10
            , Border.color white
            , Border.width 2
            ]
        <|
            column []
                [ row [ paddingXY 0 10 ]
                    [ Input.text [ Font.color black ]
                        { onChange = UpdateOffer
                        , text = model.currentOffer
                        , placeholder = Nothing
                        , label = Input.labelAbove [] <| text ("Make an offer on " ++ itemString)
                        }
                    ]
                , el [ Font.color white, centerX, paddingEach { top = 0, bottom = 25, left = 0, right = 0 } ] <| text messageText
                , row [ alignRight, spacingXY 5 0 ]
                    (showOfferButtons model.offerMessage)
                ]

    else
        none


showOfferButtons : OfferMessage -> List (Element Msg)
showOfferButtons offerMessage =
    if offerMessage /= AcceptedOfferMessage then
        [ Input.button
            [ Background.color blue
            , Element.focused
                [ Background.color blue ]
            , Font.color white
            , alignRight
            ]
            { onPress = Just CancelOffer
            , label = el [] <| text "Cancel"
            }
        , Input.button
            [ Background.color blue
            , Element.focused
                [ Background.color blue ]
            , Font.color white
            , alignRight
            ]
            { onPress = Just MakeOffer
            , label = el [] <| text "Offer"
            }
        ]

    else
        [ Input.button
            [ Background.color blue
            , Element.focused
                [ Background.color blue ]
            , Font.color white
            , alignRight
            ]
            { onPress = Just CancelOffer
            , label = el [] <| text "OK"
            }
        ]



---- VIEW HOLDING PAGE ----


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



---- VIEW SHOP PAGE ----


shopPage : Model -> Browser.Document Msg
shopPage model =
    { title = "Dungeon of Doom - Shop"
    , body =
        [ layout [ Background.color black, inFront (offerDialog model) ] <|
            column [ width fill, paddingXY 0 100 ]
                ((el [ Font.size 20, Font.color white ] <| text ("Gold Coins: " ++ String.fromInt (Character.getGold model.character)))
                    :: printArmouryItems model
                    ++ saveCharacterButton
                )
        ]
    }


saveCharacterButton : List (Element Msg)
saveCharacterButton =
    [ el [ Font.alignRight ] <|
        Input.button
            [ Background.color blue
            , Element.focused
                [ Background.color blue ]
            ]
            { onPress = Just (OptionSelected SaveCharacter)
            , label = text "Save Character"
            }
    ]


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
    [ table [ Font.size 20, Font.color white, spacingXY 15 5 ]
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
                                    { onPress = Just (BargainForItem item.item)
                                    , label = text "Bargain"
                                    }
              }
            ]
        }
    ]



---- VIEW CHARACTER GENERATOR PAGE ----


characterGeneratorPage : Character -> Browser.Document Msg
characterGeneratorPage character =
    { title = "Dungeon of Doom - Character Generator"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                ([ el [ Font.size 20, Font.color white ] <| text (Character.getClassString character)
                 ]
                    ++ List.map (printStats (Character.getStatPoints character)) (Character.getStatList character)
                    ++ [ buildStatElement "Stat Points" (Character.getStatPoints character)
                       , buildStatElement "Experience" (Character.getXP character)
                       , buildStatElement "Gold" (Character.getGold character)
                       , backButton
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


buildStatElement : String -> Int -> Element msg
buildStatElement name val =
    el [ Font.size 20, Font.color white ] <|
        text (name ++ ": " ++ String.fromInt val)


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



---- VIEW SAVE CHARACTER PAGE ----


saveCharacterScreen : Character -> Browser.Document Msg
saveCharacterScreen character =
    { title = "Dungeon of Doom - Save Character"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                [ Input.text [ Font.size 20, Font.color black, width (fill |> minimum 300 |> maximum 300) ]
                    { onChange = UpdateName
                    , text = Maybe.withDefault "" (Character.getName character)
                    , placeholder = Just (Input.placeholder [ Font.size 20, Font.color black ] <| text "Enter character name")
                    , label = Input.labelLeft [ Font.size 20, Font.color white ] <| text "Name: "
                    }
                , Input.button
                    [ Background.color blue
                    , Element.focused
                        [ Background.color blue ]
                    ]
                    { onPress = Just SaveCharacterToDB
                    , label = text "Save"
                    }
                ]
        ]
    }



---- VIEW THE GAME PAGE ----


theGamePage : Model -> Browser.Document Msg
theGamePage model =
    { title = "Dungeon of Doom - The Game"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                [ Dropdown.view dropdownConfig model.dropdownState model.characterList
                    |> el [ Font.color white ]
                ]
        ]
    }


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
        itemToPrompt item =
            Element.text item

        itemToElement selected highlighted item =
            Element.text item
    in
    Dropdown.basic DropdownMsg OptionPicked itemToPrompt itemToElement



---- COLOR SETTING ----


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


red : Color
red =
    rgb255 255 0 0



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


bargainPrice : Random.Generator Int
bargainPrice =
    Random.int 1 3


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



---- PORTS ----


port saveCharacterToDB : String -> Cmd msg


port retrieveCharacterList : () -> Cmd msg


port retrieveCharacter : String -> Cmd msg


port characterListReceiver : (List String -> msg) -> Sub msg


port characterReceiver : (String -> msg) -> Sub msg
