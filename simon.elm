import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random 
import Array

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg 
  = StartOver
  | NewValue Int
  | ClickCero  
  | ClickUno
  | ClickDos
  | ClickTres

type alias Model =
  { 
    sequence  : Array.Array Int,
    userMoveNumber : Int,
    message : String
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewValue newValue ->
      ({model | sequence = Array.push newValue model.sequence}, Cmd.none)

    StartOver ->
      init

    ClickCero ->
      checkMove 0 model

    ClickUno ->
      checkMove 1 model

    ClickDos ->
      checkMove 2 model

    ClickTres ->
      checkMove 3 model


checkMove : Int -> Model -> (Model, Cmd Msg)
checkMove userMove model =
  let
    ok = Array.get model.userMoveNumber model.sequence
          |> Maybe.map (\i -> i == userMove)
          |> Maybe.withDefault False
  in
    if ok then
      goodMove model
    else
      wrongMove model


goodMove : Model -> (Model, Cmd Msg)
goodMove model =
  let reachedEndOfSequence = model.userMoveNumber+1 == Array.length model.sequence

  in 
    if reachedEndOfSequence then
      ({ model | userMoveNumber = 0, message = "Keep Going!"}, Random.generate NewValue (Random.int 0 3))
    else
      ({ model | userMoveNumber = model.userMoveNumber+1, message = "Keep Going!" }, Cmd.none)

wrongMove : Model -> (Model, Cmd Msg)
wrongMove model = ({ model |  message = "Wrong! Your Score was " ++ toString (model.userMoveNumber), userMoveNumber = 0, sequence = Array.empty }, Cmd.none)


init : (Model, Cmd Msg)
init =
  ({sequence = Array.empty, userMoveNumber=0, message=""}, Random.generate NewValue (Random.int 0 3))


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick StartOver ] [ text "StartOver" ]
    , div [] [ text (toString model) ]
    , button [ onClick (ClickCero)] [ text "0" ]
    , button [ onClick (ClickUno) ] [ text "1" ]
    , button [ onClick (ClickDos) ] [ text "2" ]
    , button [ onClick (ClickTres) ] [ text "3" ]
    ]