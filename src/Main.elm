module Main exposing (main)

import Browser
import Character exposing (Character)
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


type Page
    = MenuPage
    | DungeonGenerator
    | CharacterGenerator
    | TheGame


type alias Model =
    { currPage : Page
    , character : Character
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = MenuPage
      , character = Character.newCharacter Nothing
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
    | IncrementStat Character.CharacterStat
    | DecrementStat Character.CharacterStat
    | UpdateName String


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
            ( { model | character = Character.incrementCharacterStat model.character characterStat }, Cmd.none )

        DecrementStat characterStat ->
            ( { model | character = Character.decrementCharacterStat model.character characterStat }, Cmd.none )

        UpdateName newName ->
            ( { model | character = Character.setName model.character newName }, Cmd.none )


optionUpdate : Model -> Page -> Model
optionUpdate model selected =
    { model | currPage = selected }


updateModelStats : Model -> List Int -> Model
updateModelStats model statList =
    { model | character = Character.updateCharacterStats model.character statList }



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
                       , backButton
                       , rerollButton
                       ]
                )
        ]
    }


printStats : Int -> ( String, ( Character.CharacterStat, Int ) ) -> Element Msg
printStats statPoints ( statString, ( characterStat, val ) ) =
    row []
        [ buildStatElement statString val
        , adjustButtons statPoints characterStat val
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
