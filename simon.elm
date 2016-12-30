module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Random
import Array
import Animation
import Color
import Task
import Time exposing (second)
import Css exposing (..)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Button =
    { label :
        String
        --for debugging purposes
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
    { sequence : Array.Array Int
    , userMoveNumber : Int
    , message : String
    , buttons : List Button
    }



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
        NewValue newValue ->
            let
                updatedModel =
                    { model | sequence = Array.push newValue model.sequence }
            in
                update (Play 0) updatedModel

        Play index ->
            case Array.get index model.sequence of
                Just buttonToAnimate ->
                    let
                        ( newModel, cmd ) =
                            (update (AnimateActive buttonToAnimate index) model)
                    in
                        if (reachedEndOfSequence index model.sequence) then
                            ( { newModel | message = "Playing " ++ (toString buttonToAnimate) }, Cmd.none )
                        else
                            andThen ( { newModel | message = "Playing " ++ (toString buttonToAnimate) }, cmd ) (update (Play (index + 1)))

                Nothing ->
                    ( { model | message = "Unexpected error: index out of range in Play function" }, Cmd.none )

        StartOver ->
            init

        CheckMove buttonNumber ->
            let
                ok =
                    Array.get model.userMoveNumber model.sequence
                        |> Maybe.map (\i -> i == buttonNumber)
                        |> Maybe.withDefault False
            in
                if ok then
                    andThen (goodMove model) (update (AnimateActive buttonNumber 0))
                else
                    andThen (wrongMove model) (update (AnimateActive buttonNumber 0))

        Animate time ->
            ( { model
                | buttons =
                    List.map
                        (onStyle (Animation.update time))
                        model.buttons
              }
            , Cmd.none
            )

        AnimateActive indexButton delay ->
            let
                currentButtonColor =
                    Array.get indexButton (Array.fromList model.buttons)
                        |> Maybe.map (\button -> button.color)
                        |> Maybe.withDefault Color.white
            in
                ( onButtonStyle model indexButton <|
                    (Animation.queue
                        [ Animation.wait (toFloat delay * 1 * second)
                        , Animation.to
                            [ Animation.backgroundColor Color.white ]
                        , Animation.set
                            [ Animation.backgroundColor currentButtonColor ]
                        ]
                    )
                , Cmd.none
                )


reachedEndOfSequence : Int -> Array.Array Int -> Bool
reachedEndOfSequence index sequence =
    (index + 1 == Array.length sequence)



--- ref: https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm


onStyle : (Animation.State -> Animation.State) -> Button -> Button
onStyle styleFn button =
    { button | style = styleFn button.style }


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


onButtonStyle : Model -> Int -> (Animation.State -> Animation.State) -> Model
onButtonStyle model buttonIndex fn =
    { model
        | buttons =
            onIndex buttonIndex model.buttons <|
                onStyle fn
    }


goodMove : Model -> ( Model, Cmd Msg )
goodMove model =
    if (reachedEndOfSequence model.userMoveNumber model.sequence) then
        ( { model | userMoveNumber = 0, message = "Yay! Next Level!" }, Random.generate NewValue (Random.int 0 3) )
    else
        ( { model | userMoveNumber = model.userMoveNumber + 1, message = "Keep Going!" }, Cmd.none )


wrongMove : Model -> ( Model, Cmd Msg )
wrongMove model =
    ( { model | message = "Wrong!! You lose :( ", userMoveNumber = 0, sequence = Array.empty }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { sequence = Array.empty
      , userMoveNumber = 0
      , message = ""
      , buttons =
            [ { label = "0"
              , onClickAction = CheckMove 0
              , style = Animation.style [ Animation.backgroundColor Color.red ]
              , color = Color.red
              }
            , { label = "1"
              , onClickAction = CheckMove 1
              , style = Animation.style [ Animation.backgroundColor Color.green ]
              , color = Color.green
              }
            , { label = "2"
              , onClickAction = CheckMove 2
              , style = Animation.style [ Animation.backgroundColor Color.blue ]
              , color = Color.blue
              }
            , { label = "3"
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


styles =
    Css.asPairs >> Html.Attributes.style


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
        , button [ onClick StartOver ] [ Html.text "StartOver" ]
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
