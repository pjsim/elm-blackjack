module Rules exposing (..)

import Deck exposing (Card)


type GameState
    = Playing
    | Staying
    | Won
    | Lost


calculateScore : GameState -> List Card -> List Card -> GameState
calculateScore game_state player dealer =
    let
        player_ranks =
            player
                |> List.map .rank

        player_result =
            player_ranks
                |> List.foldr (+) 0

        dealer_ranks =
            dealer
                |> List.map .rank

        dealer_result =
            dealer_ranks
                |> List.foldr (+) 0
    in
        if player_result > 21 then
            Lost
        else if player_result == 21 then
            Won
        else if dealer_result > 21 then
            Won
        else if player_result > dealer_result && game_state == Staying then
            Won
        else if dealer_result > player_result then
            Lost
        else
            game_state


displayScore : List Card -> String
displayScore hand =
    let
        ranks =
            hand
                |> List.map .rank

        result =
            ranks
                |> List.foldr (+) 0
    in
        -- Need to define this rule better
        if result > 21 then
            toString result ++ ": Bust"
        else if result == 21 then
            toString result ++ ": Blackjack"
        else
            toString result
