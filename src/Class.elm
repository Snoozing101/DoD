module Class exposing (CharacterClass(..)
                       , classToString
                       )

type CharacterClass
    = Wanderer
    | Cleric
    | Magician
    | Warrior
    | Barbarian


classToString : CharacterClass -> String
classToString class =
    case class of
        Wanderer ->
            "Wanderer"

        Cleric ->
            "Cleric"

        Magician ->
            "Magician"

        Warrior ->
            "Warrior"

        Barbarian ->
            "Barbarian"


