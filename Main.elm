module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Random


main : Program Never Model Msg
main =
    program { init = createModel, update = update, view = view, subscriptions = \_ -> Sub.none }



-- make rank a union type?


type alias Card =
    { index : Int
    , suit : String
    , rank : Int
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
    Card index rank suit


type alias Model =
    { deck : List Card
    , player_hand : List Card
    , dealer_hand : List Card
    }


type Msg
    = NoOp
    | Shuffle (List Int)
    | ShuffleDeck
    | SortDeck


createModel : ( Model, Cmd Msg )
createModel =
    ( Model generateDeck [] []
      -- , randomList Shuffle 52
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck model.deck xs
            in
                Model newDeck [] [] ! []

        ShuffleDeck ->
            ( model, randomList Shuffle 52 )

        SortDeck ->
            ( { model | deck = model.deck |> List.sortBy .index }, Cmd.none )


shuffleDeck : List Card -> List comparable -> List Card
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text <| toString <| List.map displayCard model.deck ]
        , button [ onClick ShuffleDeck ] [ text "Shuffle" ]
        , button [ onClick SortDeck ] [ text "Sort" ]
        ]


displayCard : Card -> String
displayCard card =
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
    Random.int 1 52
        |> Random.list len
        |> Random.generate msg
