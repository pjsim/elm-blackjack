module Deck exposing (..)

import Random


-- MODEL


type alias Card =
    { index : Int
    , suit : String
    , rank : Int
    , facing : Facing
    }


generateDeck : List Card
generateDeck =
    let
        deckIndex =
            12 * 4 |> List.range 1

        suits =
            [ "Spades", "Clubs", "Diamonds", "Hearts" ] |> List.repeat 12 |> List.concat |> List.sort

        ranks =
            List.range 2 13 |> List.repeat 4 |> List.concat
    in
        List.map3 createCard deckIndex suits ranks


createCard : Int -> String -> Int -> Card
createCard index rank suit =
    Card index rank suit Down


type Facing
    = Down
    | Up



deal : List Card -> ( Maybe Card, Maybe (List Card) )
deal deck =
    let
        dealtCard =
            List.head deck

        restOfDeck =
            List.tail deck
    in
        ( dealtCard, restOfDeck )


shuffleDeck : List Card -> List comparable -> List Card
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


displayCard : Card -> String
displayCard card =
    if card.facing == Down then
        "CARD"
    else
        case card.rank of
            13 ->
                "Ace of " ++ card.suit

            12 ->
                "King of " ++ card.suit

            11 ->
                "Queen of " ++ card.suit

            10 ->
                "Jack of " ++ card.suit

            _ ->
                toString card.rank ++ " of " ++ card.suit

