module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Random


-- Need to put deck stuff in own module exposing creating, dealing, shuffling and sorting methods
-- Need to use svg for cards and have Main focus on game and playing states
-- Put card stuff in own file and maybe game rules in own file


main : Program Never Model Msg
main =
    program { init = newGame, update = update, view = view, subscriptions = \_ -> Sub.none }



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


type GameState
    = Playing
    | Won
    | Lost


type alias Model =
    { deck : List Card
    , player_hand : List Card
    , dealer_hand : List Card
    , game_state : GameState
    }


type Msg
    = Shuffle (List Int)
    | ShuffleDeck
    | SortDeck
    | Hit
    | Stay
    | Restart


newGame : ( Model, Cmd Msg )
newGame =
    ( Model generateDeck [] [] Playing
    , randomList Shuffle 48
    )



-- UPDATE


calculateScore : List Card -> List Card -> GameState
calculateScore player dealer =
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
        else if dealer_result > player_result then
            Lost
        else
            Playing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck model.deck xs
            in
                Model newDeck [] [] Playing ! []

        ShuffleDeck ->
            ( model, randomList Shuffle 48 )

        SortDeck ->
            ( { model | deck = model.deck |> List.sortBy .index }, Cmd.none )

        Hit ->
            let
                ( dealtCard, restOfDeck ) =
                    deal model.deck
            in
                case dealtCard of
                    Just card ->
                        case restOfDeck of
                            Just cards ->
                                let
                                    newHands =
                                        { model | player_hand = { card | facing = Up } :: model.player_hand, deck = cards }
                                in
                                    ( { newHands | game_state = calculateScore newHands.player_hand newHands.dealer_hand }, Cmd.none )

                            Nothing ->
                                model ! []

                    Nothing ->
                        model ! []

        Stay ->
            let
                ( dealtCard, restOfDeck ) =
                    deal model.deck
            in
                case dealtCard of
                    Just card ->
                        case restOfDeck of
                            Just cards ->
                                let
                                    newHands =
                                        { model | dealer_hand = { card | facing = Up } :: model.dealer_hand, deck = cards }
                                in
                                    ( { newHands | game_state = calculateScore newHands.player_hand newHands.dealer_hand }, Cmd.none )

                            Nothing ->
                                model ! []

                    Nothing ->
                        model ! []

        Restart ->
            newGame


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Deck" ]
        , h4 [] [ text <| toString <| List.map displayCard model.deck ]
        , h3 [] [ text "Player Hand" ]
        , h4 [] [ text <| toString <| List.map displayCard model.player_hand ]
        , h5 [] [ text <| displayScore model.player_hand ]
        , h3 [] [ text "Dealer Hand" ]
        , h4 [] [ text <| toString <| List.map displayCard model.dealer_hand ]
        , h5 [] [ text <| displayScore model.dealer_hand ]
          -- , button [ onClick ShuffleDeck ] [ text "Shuffle" ]
          -- , button [ onClick SortDeck ] [ text "Sort" ]
        , h2 [] [ text <| toString model.game_state ]
        , div
            []
            [ if model.game_state == Playing then
                div []
                    [ button [ onClick Hit ] [ text "Hit" ]
                    , button [ onClick Stay ] [ text "Stay" ]
                    ]
              else
                div []
                    [ button [ onClick Restart ] [ text "Play Again" ]
                    ]
            ]
        ]


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


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 1 48
        |> Random.list len
        |> Random.generate msg
