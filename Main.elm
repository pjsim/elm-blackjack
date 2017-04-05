module Main exposing (..)

import Deck exposing (Card, generateDeck, deal, shuffleDeck, Facing(..), displayCard)
import Rules exposing (GameState(..), calculateScore, displayScore)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Svg
import Svg.Attributes as SvgAtt


-- Need to put deck stuff in own module exposing creating, dealing, shuffling and sorting methods
-- Need to use svg for cards and have Main focus on game and playing states
-- Put card stuff in own file and maybe game rules in own file


main : Program Never Model Msg
main =
    program { init = newGame, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


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


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 1 48
        |> Random.list len
        |> Random.generate msg


-- UPDATE


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
                                    ( { newHands | game_state = calculateScore Playing newHands.player_hand newHands.dealer_hand }, Cmd.none )

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
                                    ( { newHands | game_state = calculateScore Staying newHands.player_hand newHands.dealer_hand }, Cmd.none )

                            Nothing ->
                                model ! []

                    Nothing ->
                        model ! []

        Restart ->
            newGame



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
        , Svg.svg
            [ SvgAtt.width "1200", SvgAtt.height "1200", SvgAtt.viewBox "0 0 1200 1200" ]
            [ Svg.g []
                [ Svg.rect
                    [ SvgAtt.x "10"
                    , SvgAtt.y "10"
                    , SvgAtt.width "85"
                    , SvgAtt.height "100"
                    , SvgAtt.rx "10"
                    , SvgAtt.ry "10"
                    , SvgAtt.fill "white"
                    , SvgAtt.stroke "black"
                    , SvgAtt.strokeWidth "4"
                    ]
                    []
                , Svg.text_ [ SvgAtt.textAnchor "middle", SvgAtt.transform "translate(70,35)" ] [ Svg.text "2 C" ]
                ]
            , Svg.g [ SvgAtt.transform "translate(100,0)" ]
                [ Svg.rect
                    [ SvgAtt.x "10"
                    , SvgAtt.y "10"
                    , SvgAtt.width "85"
                    , SvgAtt.height "100"
                    , SvgAtt.rx "10"
                    , SvgAtt.ry "10"
                    , SvgAtt.style "fill:white;stroke:black;stroke-width:4"
                    ]
                    []
                , Svg.text_ [ SvgAtt.textAnchor "middle", SvgAtt.transform "translate(70,35)" ] [ Svg.text "3 C" ]
                ]
            ]
        ]


svgCard : Card -> Html Msg
svgCard card =
    Svg.svg
        [ SvgAtt.width "1200", SvgAtt.height "1200", SvgAtt.viewBox "0 0 1200 1200" ]
        [ Svg.g []
            [ Svg.rect
                [ SvgAtt.x "10"
                , SvgAtt.y "10"
                , SvgAtt.width "85"
                , SvgAtt.height "100"
                , SvgAtt.rx "10"
                , SvgAtt.ry "10"
                , SvgAtt.style "fill:white;stroke:black;stroke-width:4"
                ]
                []
            , Svg.text_ [ SvgAtt.textAnchor "middle", SvgAtt.transform "translate(70,35)" ] [ Svg.text <| (toString card.rank) ++ " " ++ card.suit ]
            ]
        ]


