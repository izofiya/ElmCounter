module Main exposing (Item, Model, Msg(..), addItem, decrementAllCounters, init, main, removeItem, subscriptions, update, updateNthCounter, view, viewCounter, viewCounters)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)
import List
import Platform exposing (Program)
import Time
import Tuple


type alias Item =
    { counter : Int
    , name : String
    }


type alias Model =
    { newName : String
    , items : List Item
    , deleted : List String
    }


type Msg
    = Add Int
    | Add10 Int
    | NewCounter
    | RemoveCounter Int
    | Dec
    | ChangeName String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> Dec)


init : () -> ( Model, Cmd Msg )
init () =
    ( { newName = "", items = [], deleted = [] }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick NewCounter
            , disabled <|
                String.isEmpty <|
                    String.trim model.newName
            ]
            [ text "+" ]
        , input
            [ value model.newName
            , onInput ChangeName
            ]
            []
        , viewCounters model.items
        , Html.ul [] <|
            List.map (\x -> Html.li [] [ text x ])
                model.deleted
        ]


viewCounters model =
    div [] <|
        List.indexedMap viewCounter <|
            model


viewCounter ind model =
    div []
        [ button [ onClick <| RemoveCounter ind ] [ text "x" ]
        , text <| toString <| model.counter
        , button
            [ onClick <| Add ind ]
            [ text "+" ]
        , button
            [ onClick <| Add10 ind ]
            [ text "++" ]
        , text model.name
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (\m -> ( m, Cmd.none )) <|
        case msg of
            Add ind ->
                { model
                    | items =
                        updateNthCounter
                            ((+) 1)
                            ind
                            model.items
                }

            Add10 ind ->
                { model
                    | items =
                        updateNthCounter
                            ((+) 10)
                            ind
                            model.items
                }

            NewCounter ->
                addItem model

            RemoveCounter ind ->
                { model
                    | items =
                        removeItem ind model.items
                }

            Dec ->
                decrementAllCounters model

            ChangeName n ->
                { model
                    | newName = n
                }


updateNthCounter f ind =
    List.indexedMap
        (\i x ->
            if i == ind then
                { x | counter = f x.counter }

            else
                x
        )


addItem model =
    { model
        | newName = ""
        , items =
            { counter = 10, name = String.trim model.newName }
                :: model.items
    }


removeItem ind model =
    model
        |> List.indexedMap Tuple.pair
        |> List.filter
            (\i -> Tuple.first i /= ind)
        |> List.map Tuple.second


decrementAllCounters model =
    { model
        | items =
            model.items
                |> List.map
                    (\x -> { x | counter = x.counter - 1 })
                |> List.filter (\x -> x.counter >= 0)
        , deleted =
            List.append
                model.deleted
                (model.items
                    |> List.filter (\x -> x.counter == 0)
                    |> List.map .name
                )
    }
