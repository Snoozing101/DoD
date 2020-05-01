module Main exposing (main)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
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


type CharacterStat
    = Strength
    | Vitality
    | Agility
    | Intelligence
    | Luck
    | Aura
    | Morality


characterStatToString : CharacterStat -> String
characterStatToString stat =
    case stat of
        Strength ->
            "Strength"

        Vitality ->
            "Vitality"

        Agility ->
            "Agility"

        Intelligence ->
            "Intelligence"

        Luck ->
            "Luck"

        Aura ->
            "Aura"

        Morality ->
            "Morality"


baseStatModifier : Int -> Int
baseStatModifier stat =
    stat + 2


type Page
    = MenuPage
    | DungeonGenerator
    | CharacterGenerator
    | TheGame


type CharacterClass
    = Wanderer
    | Cleric
    | Mage
    | Warrior
    | Barbarian


classToString : CharacterClass -> String
classToString class =
    case class of
        Wanderer ->
            "Wanderer"

        Cleric ->
            "Cleric"

        Mage ->
            "Mage"

        Warrior ->
            "Warrior"

        Barbarian ->
            "Barbarian"


type alias Character =
    { class : CharacterClass
    , name : String
    , stats : List ( CharacterStat, Int )
    , experience : Int
    , statPoints : Int
    }


type alias Model =
    { currPage : Page
    , character : Character
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = MenuPage
      , character =
            Character Wanderer
                "Apathy"
                [ ( Strength, 0 )
                , ( Vitality, 0 )
                , ( Agility, 0 )
                , ( Intelligence, 0 )
                , ( Luck, 0 )
                , ( Aura, 0 )
                , ( Morality, 0 )
                ]
                0
                0
      }
    , Random.generate NewCharacter newStats
    )



---- SUBSCRITPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type Msg
    = OptionSelected Page
    | NewCharacter (List Int)
    | ReRollCharacter
    | IncrementStat CharacterStat
    | DecrementStat CharacterStat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionSelected page ->
            ( optionUpdate model page, Cmd.none )

        NewCharacter stats ->
            ( updateModelStats model stats, Cmd.none )

        ReRollCharacter ->
            ( model, Random.generate NewCharacter newStats )

        IncrementStat characterStat ->
            ( incrementCharacterStat model characterStat, Cmd.none )

        DecrementStat characterStat ->
            ( decrementCharacterStat model characterStat, Cmd.none )


incrementCharacterStat : Model -> CharacterStat -> Model
incrementCharacterStat model characterStat =
    let
        currCharacter =
            model.character

        oldStats =
            currCharacter.stats

        updatedStats =
            List.map (\x -> incrementStat x characterStat) oldStats

        updatedCharacter =
            { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints - 1 }
    in
    { model | character = updatedCharacter }


incrementStat : ( CharacterStat, Int ) -> CharacterStat -> ( CharacterStat, Int )
incrementStat ( characterStat, val ) matchStat =
    if characterStat == matchStat then
        ( characterStat, val + 1 )

    else
        ( characterStat, val )


decrementCharacterStat : Model -> CharacterStat -> Model
decrementCharacterStat model characterStat =
    let
        currCharacter =
            model.character

        oldStats =
            currCharacter.stats

        updatedStats =
            List.map (\x -> decrementStat x characterStat) oldStats

        updatedCharacter =
            { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints + 1 }
    in
    { model | character = updatedCharacter }


decrementStat : ( CharacterStat, Int ) -> CharacterStat -> ( CharacterStat, Int )
decrementStat ( characterStat, val ) matchStat =
    if characterStat == matchStat then
        ( characterStat, val - 1 )

    else
        ( characterStat, val )


optionUpdate : Model -> Page -> Model
optionUpdate model selected =
    { model | currPage = selected }


updateModelStats : Model -> List Int -> Model
updateModelStats model statList =
    { model | character = updateCharacterStats model.character statList }


updateCharacterStats : Character -> List Int -> Character
updateCharacterStats character randomList =
    let
        ( statPoints, statList ) =
            splitRandomList randomList
    in
    { character | stats = updateStats character.stats statList, statPoints = statPoints, experience = 1 }


splitRandomList : List Int -> ( Int, List Int )
splitRandomList randomList =
    case randomList of
        statPoints :: statList ->
            ( statPoints, statList )

        [] ->
            ( 0, [] )


updateStats : List ( CharacterStat, Int ) -> List Int -> List ( CharacterStat, Int )
updateStats oldCharacterStats statList =
    let
        updater =
            setNewStatValue statList
    in
    List.indexedMap updater oldCharacterStats


setNewStatValue : List Int -> Int -> ( CharacterStat, Int ) -> ( CharacterStat, Int )
setNewStatValue statList index oldStat =
    let
        newArray =
            Array.fromList statList

        newValue =
            Maybe.withDefault 0 (Array.get index newArray)

        statType =
            Tuple.first oldStat
    in
    ( statType, baseStatModifier newValue )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.currPage of
        MenuPage ->
            menuPage

        DungeonGenerator ->
            holdingPage "Dungeon Generator"

        CharacterGenerator ->
            characterGeneratorPage model

        TheGame ->
            holdingPage "The Game"


menuPage : Browser.Document Msg
menuPage =
    { title = "Dungeon of Doom"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                [ column [ centerX, Border.width 4, Border.color white ]
                    [ el [ paddingXY 100 10, Font.color green, Font.size 40 ] <| text "Dungeon of Doom"
                    , column [ width fill, Font.color white, paddingXY 40 10, Border.widthEach { top = 4, left = 0, right = 0, bottom = 0 }, Border.color white, spacing 5 ]
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


characterGeneratorPage : Model -> Browser.Document Msg
characterGeneratorPage model =
    let
        currCharacter =
            model.character
    in
    { title = "Dungeon of Doom - Character Generator"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                ([ el [ Font.size 40, Font.color white ] <|
                    text model.character.name
                 , el [ Font.size 20, Font.color white ] <| text (classToString model.character.class)
                 ]
                    ++ List.map (printStats currCharacter.statPoints) currCharacter.stats
                    ++ [ buildStatElement "Stat Points" currCharacter.statPoints
                       , buildStatElement "Experience" currCharacter.experience
                       , backButton
                       , rerollButton
                       ]
                )
        ]
    }


printStats : Int -> ( CharacterStat, Int ) -> Element Msg
printStats statPoints ( characterStat, val ) =
    row []
        [ buildStatElement (characterStatToString characterStat) val
        , adjustButtons statPoints characterStat val
        ]


adjustButtons : Int -> CharacterStat -> Int -> Element Msg
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

processIncrement : Int -> CharacterStat -> Maybe Msg
processIncrement statPoints characterStat =
    if statPoints > 0 then
        Just (IncrementStat characterStat)
    else
        Nothing


processDecrement : Int -> CharacterStat -> Maybe Msg
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
