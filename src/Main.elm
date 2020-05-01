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
    | Experience
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

        Experience ->
            "Experience"

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
                , ( Experience, 0 )
                , ( Luck, 0 )
                , ( Aura, 0 )
                , ( Morality, 0 )
                ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionSelected page ->
            ( optionUpdate model page, Cmd.none )

        NewCharacter stats ->
            ( updateModelStats model stats, Cmd.none )

        ReRollCharacter ->
            ( model, Random.generate NewCharacter newStats )


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
    { character | stats = updateStats character.stats statList, statPoints = statPoints }


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
    if statType /= Experience then
        ( statType, baseStatModifier newValue )

    else
        ( statType, 1 )



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
    { title = "Dungeon of Doom - Character Generator"
    , body =
        [ layout [ Background.color black ] <|
            column [ width fill, paddingXY 0 100 ]
                ([ el [ Font.size 40, Font.color white ] <|
                    text model.character.name
                 , el [ Font.size 20, Font.color white ] <| text (classToString model.character.class)
                 ]
                    ++ List.map printStats model.character.stats
                    ++ [ printStatPoints model.character.statPoints
                       , backButton
                       , rerollButton
                       ]
                )
        ]
    }


printStatPoints : Int -> Element msg
printStatPoints statPoints =
    el [ Font.size 20, Font.color white ] <|
        text ("Stat Points: " ++ String.fromInt statPoints)


printStats : ( CharacterStat, Int ) -> Element msg
printStats ( name, val ) =
    el [ Font.size 20, Font.color white ] <|
        text (characterStatToString name ++ ": " ++ String.fromInt val)


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
