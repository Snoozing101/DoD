module Main exposing (main)

import Array
import Browser
import Dict exposing (Dict)
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
    , stats : Dict String ( CharacterStat, Int )
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
                (Dict.fromList
                    [ makeStatDictEntry Strength 0
                    , makeStatDictEntry Vitality 0
                    , makeStatDictEntry Agility 0
                    , makeStatDictEntry Intelligence 0
                    , makeStatDictEntry Luck 0
                    , makeStatDictEntry Aura 0
                    , makeStatDictEntry Morality 0
                    ]
                )
                0
                0
      }
    , Random.generate NewCharacter newStats
    )


makeStatDictEntry : CharacterStat -> Int -> ( String, ( CharacterStat, Int ) )
makeStatDictEntry characterStat val =
    let
        statString =
            characterStatToString characterStat
    in
    ( statString, ( characterStat, val ) )



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
            Dict.update (characterStatToString characterStat) (Maybe.map incrementStat) oldStats

        updatedClass =
            updateClass updatedStats

        updatedCharacter =
            { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints - 1, class = updatedClass }
    in
    { model | character = updatedCharacter }


incrementStat : ( CharacterStat, Int ) -> ( CharacterStat, Int )
incrementStat ( characterStat, num ) =
    ( characterStat, num + 1 )


decrementCharacterStat : Model -> CharacterStat -> Model
decrementCharacterStat model characterStat =
    let
        currCharacter =
            model.character

        oldStats =
            currCharacter.stats

        updatedStats =
            Dict.update (characterStatToString characterStat) (Maybe.map decrementStat) oldStats

        updatedClass =
            updateClass updatedStats

        updatedCharacter =
            { currCharacter | stats = updatedStats, statPoints = currCharacter.statPoints + 1, class = updatedClass }
    in
    { model | character = updatedCharacter }


decrementStat : ( CharacterStat, Int ) -> ( CharacterStat, Int )
decrementStat ( characterStat, num ) =
    ( characterStat, num - 1 )


updateClass : Dict String ( CharacterStat, Int ) -> CharacterClass
updateClass stats =
    let
        intelligence =
            getStatValue stats Intelligence

        morality =
            getStatValue stats Morality

        aura =
            getStatValue stats Aura

        strength =
            getStatValue stats Strength

        vitality =
            getStatValue stats Vitality

        agility =
            getStatValue stats Agility
    in
    if intelligence > 6 && morality > 7 then
        Cleric

    else if intelligence > 8 && aura > 7 then
        Mage

    else if strength > 7 && morality > 5 && (strength + vitality) > 10 then
        Warrior

    else if strength > 8 && (vitality + agility) > 12 && morality < 6 then
        Barbarian

    else
        Wanderer


getStatValue : Dict String ( CharacterStat, Int ) -> CharacterStat -> Int
getStatValue stats characterStat =
    let
        statEntry =
            Dict.get (characterStatToString characterStat) stats
    in
    case statEntry of
        Just ( _, val ) ->
            val

        Nothing ->
            0


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


updateStats : Dict String ( CharacterStat, Int ) -> List Int -> Dict String ( CharacterStat, Int )
updateStats oldCharacterStats statList =
    let
        updater =
            setNewStatValue statList

        oldCharacterStatList =
            Dict.toList oldCharacterStats
    in
    List.indexedMap updater oldCharacterStatList
        |> Dict.fromList


setNewStatValue : List Int -> Int -> ( String, ( CharacterStat, Int ) ) -> ( String, ( CharacterStat, Int ) )
setNewStatValue statList index oldStat =
    let
        newArray =
            Array.fromList statList

        newValue =
            Maybe.withDefault 0 (Array.get index newArray)

        statString =
            Tuple.first oldStat

        statType =
            Tuple.first (Tuple.second oldStat)
    in
    ( statString, ( statType, baseStatModifier newValue ) )



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
                    ++ List.map (printStats currCharacter.statPoints) (Dict.toList currCharacter.stats)
                    ++ [ buildStatElement "Stat Points" currCharacter.statPoints
                       , buildStatElement "Experience" currCharacter.experience
                       , backButton
                       , rerollButton
                       ]
                )
        ]
    }


printStats : Int -> ( String, ( CharacterStat, Int ) ) -> Element Msg
printStats statPoints ( statString, ( characterStat, val ) ) =
    row []
        [ buildStatElement statString val
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
