module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random
import Array
import Animation
import Color
import Time exposing (second)
import Css exposing (..)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Button =
    { label :
        String
        --for debugging purposes, not used in the program
    , onClickAction : Msg
    , style : Animation.State
    , color : Color.Color
    }


type Msg
    = StartOver
    | NewValue Int
    | CheckMove Int
    | Animate Animation.Msg
    | AnimateActive Int Int
    | Play Int


type alias Model =
    { sequence :
        Array.Array Int
        -- Contains the right sequence that needs to be played by the user in each level
    , userMoveNumber :
        Int
        -- Index of the last correct move that the user clicked
    , message :
        String
        -- Message to user
    , buttons :
        List Button
        --
    }



-- This is a pattern that I use often, I extracted the behaviour into this function for readability
-- ref: http://folkertdev.nl/blog/task-perform-with-task-succeed/


andThen : ( Model, Cmd msg ) -> (Model -> ( Model, Cmd msg )) -> ( Model, Cmd msg )
andThen ( beginModel, cmd1 ) advance =
    let
        ( newModel, cmd2 ) =
            advance beginModel
    in
        ( newModel, Cmd.batch [ cmd1, cmd2 ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- New randomly generated value added to the sequence after a level is completed
        -- Since is the beggining of a new level, all sequence (including the new value)
        -- needs to be played starting from the beggining (index = 0)
        NewValue newValue ->
            andThen ( { model | sequence = Array.push newValue model.sequence }, Cmd.none ) (update (Play 0))

        -- This function animates the whole sequence by playing each element in the sequence in order
        -- This is a recursive function that "ends" when the index reaches the last element of the sequence (index = sequence's length + 1)
        -- this is why the index is always in range, and the Nothing branch never gets executed.
        Play index ->
            case Array.get index model.sequence of
                Just buttonToAnimate ->
                    let
                        ( newModel, cmd ) =
                            (update (AnimateActive buttonToAnimate index) model)
                    in
                        if (reachedEndOfSequence index model.sequence) then
                            ( newModel, Cmd.none )
                        else
                            andThen ( newModel, cmd ) (update (Play (index + 1)))

                Nothing ->
                    ( { model | message = "Unexpected error: index out of range in Play function" }, Cmd.none )

        -- This resets the whole game
        StartOver ->
            init

        -- This checks if the button clicked by the user is the one that she is supposed to click in this turn
        -- The button gets animated to simulate a physical button pressed
        CheckMove buttonNumber ->
            let
                correctMove =
                    Array.get model.userMoveNumber model.sequence
                        |> Maybe.map (\i -> i == buttonNumber)
                        |> Maybe.withDefault False
            in
                if correctMove then
                    andThen (goodMove model) (update (AnimateActive buttonNumber 0))
                else
                    andThen (wrongMove model) (update (AnimateActive buttonNumber 0))

        -- helper message to animate each button style
        -- ref: https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm
        Animate time ->
            ( { model
                | buttons =
                    List.map
                        (onStyle (Animation.update time))
                        model.buttons
              }
            , Cmd.none
            )

        -- Animation that simulates a physical click.
        -- This gets call when the user presses a button or when playing the whole sequence for each new level
        AnimateActive indexButton delay ->
            let
                currentButtonColor =
                    Array.get indexButton (Array.fromList model.buttons)
                        |> Maybe.map (\button -> button.color)
                        |> Maybe.withDefault Color.white
            in
                ( onButtonStyle model indexButton <|
                    (Animation.interrupt
                        [ Animation.wait (toFloat delay * 0.8 * second)
                        , Animation.to
                            [ Animation.backgroundColor Color.white ]
                        , Animation.to
                            [ Animation.backgroundColor currentButtonColor ]
                        ]
                    )
                , Cmd.none
                )


reachedEndOfSequence : Int -> Array.Array Int -> Bool
reachedEndOfSequence index sequence =
    (index + 1 == Array.length sequence)



-- This is a helper function to perform the animation on the correct button
-- ref: https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm


onStyle : (Animation.State -> Animation.State) -> Button -> Button
onStyle styleFn button =
    { button | style = styleFn button.style }



-- This is a helper function to perform the animation on the correct button
-- ref: https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm


onIndex : Int -> List a -> (a -> a) -> List a
onIndex i list fn =
    List.indexedMap
        (\j val ->
            if i == j then
                fn val
            else
                val
        )
        list



-- This is a helper function to perform the animation on the correct button
-- ref: https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm


onButtonStyle : Model -> Int -> (Animation.State -> Animation.State) -> Model
onButtonStyle model buttonIndex fn =
    { model
        | buttons =
            onIndex buttonIndex model.buttons <|
                onStyle fn
    }



-- If the move was correct we need to inform the user and (if the level was completed) generate a new random value for the sequence


goodMove : Model -> ( Model, Cmd Msg )
goodMove model =
    if (reachedEndOfSequence model.userMoveNumber model.sequence) then
        ( { model | userMoveNumber = 0, message = "Yay! Next Level!" }, Random.generate NewValue (Random.int 0 3) )
    else
        ( { model | userMoveNumber = model.userMoveNumber + 1, message = "Keep Going!" }, Cmd.none )



-- just for redability


wrongMove : Model -> ( Model, Cmd Msg )
wrongMove model =
    ( { model | message = "Wrong!! You lose :( " }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { sequence = Array.empty
      , userMoveNumber = 0
      , message = ""
      , buttons =
            [ { label = "red"
              , onClickAction = CheckMove 0
              , style = Animation.style [ Animation.backgroundColor Color.red ]
              , color = Color.red
              }
            , { label = "green"
              , onClickAction = CheckMove 1
              , style = Animation.style [ Animation.backgroundColor Color.green ]
              , color = Color.green
              }
            , { label = "blue"
              , onClickAction = CheckMove 2
              , style = Animation.style [ Animation.backgroundColor Color.blue ]
              , color = Color.blue
              }
            , { label = "yellow"
              , onClickAction = CheckMove 3
              , style = Animation.style [ Animation.backgroundColor Color.yellow ]
              , color = Color.yellow
              }
            ]
      }
    , Random.generate NewValue (Random.int 0 3)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <|
        List.map .style model.buttons



-- Helper for adding styles in a purely fuctional way
-- ref: https://github.com/rtfeldman/elm-css#approach-1-inline-styles


styles =
    Css.asPairs >> Html.Attributes.style



-- using justify-content as a property looses type-safety but the native version is not implemented yet in the elm-csslibrary
-- reason: https://github.com/rtfeldman/elm-css/issues/198
-- ref: https://github.com/rtfeldman/elm-css/#missing-css-properties


view : Model -> Html Msg
view model =
    div [ styles [ displayFlex, flexDirection column, flexWrap wrap, property "justify-content" "center", alignItems center ] ]
        [ div [] [ Html.text ("Score: " ++ toString ((Array.length model.sequence) - 1)) ]
        , div [] [ Html.text ("Message: " ++ toString model.message) ]
        , div
            [ styles
                [ width (px 200)
                , height (px 200)
                , borderRadius (px 50)
                , backgroundColor (hex "000")
                , displayFlex
                , flexDirection row
                , flexWrap wrap
                , property "justify-content" "center"
                , alignItems center
                ]
            ]
            [ div []
                [ div [] (List.map viewButton (List.take 2 model.buttons))
                , div [] (List.map viewButton (List.drop 2 model.buttons))
                ]
            ]
        , button [ onClick StartOver ] [ Html.text "Start Over" ]
        ]


viewButton : Button -> Html Msg
viewButton button =
    div
        (Animation.render button.style
            ++ [ styles
                    [ padding (px 8)
                    , width (px 40)
                    , height (px 40)
                    , borderRadius (px 10)
                    , display inlineBlock
                    , margin (px 10)
                    ]
               , onClick (button.onClickAction)
               ]
        )
        []
