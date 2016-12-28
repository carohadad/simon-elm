import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onMouseDown)
import Html.Attributes exposing (style)
import Random 
import Array
import Animation exposing (px)
import Color
import Task

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Button =
  { label : String
  , onClickAction : Msg
  , onMouseDownAction : Msg
  , style : Animation.State
  , color : Color.Color
  , colorString : String
  }

type Msg 
  = StartOver
  | NewValue Int
  | CheckMove Int  
  | Animate Animation.Msg
  | AnimateActive Int
  | Play Int


type alias Model =
  { 
    sequence  : Array.Array Int,
    userMoveNumber : Int,
    message : String,
    buttons : List Button
  }

-- idea from http://folkertdev.nl/blog/task-perform-with-task-succeed/
andThen : ( Model, Cmd msg ) -> ( Model -> ( Model, Cmd msg ) ) -> ( Model, Cmd msg ) 
andThen ( beginModel, cmd1 )  advance = 
    let 
        ( newModel, cmd2 ) = advance beginModel
    in 
        ( newModel, Cmd.batch [ cmd1, cmd2 ] ) 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewValue newValue ->
      let 
       (newModel, cmd) = update (Play 0) model
      in
        ({newModel | sequence = Array.push newValue model.sequence}, cmd)

    Play index ->
      if (index == (Array.length model.sequence)) then
        (model, Cmd.none)
      else
        let
          buttonToAnimate = Array.get index model.sequence |> Maybe.withDefault 4
          (newModel, cmd) = (update (AnimateActive buttonToAnimate) model)
        in
          andThen (newModel, cmd) (update (Play (index+1)))

    StartOver ->
      init

    CheckMove buttonNumber ->
      let
        ok = Array.get model.userMoveNumber model.sequence
          |> Maybe.map (\i -> i == buttonNumber)
          |> Maybe.withDefault False
      in
        if ok then
          goodMove model
        else
          wrongMove model

    Animate time ->
      ( { model
          | buttons =
              List.map
                  (onStyle (Animation.update time))
                  model.buttons
        }
      , Cmd.none
      )

    AnimateActive index ->
      let currentButtonColor = Array.get index (Array.fromList model.buttons)
        |> Maybe.map (\button -> button.color)
        |> Maybe.withDefault Color.white

      in 
      ( onButtonStyle model index <|
                (Animation.queue
                    [ Animation.to
                        [ Animation.backgroundColor Color.white ]
                    , Animation.set
                        [ Animation.backgroundColor currentButtonColor ]
                    ]
                )
            , Cmd.none
            )

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
onButtonStyle model index fn =
    { model
        | buttons =
            onIndex index model.buttons <|
                onStyle fn
    }


goodMove : Model -> (Model, Cmd Msg)
goodMove model =
  let reachedEndOfSequence = model.userMoveNumber+1 == Array.length model.sequence

  in 
    if reachedEndOfSequence then
      ({ model | userMoveNumber = 0, message = "Yay! Next Level!"}, Random.generate NewValue (Random.int 0 3))
    else
      ({ model | userMoveNumber = model.userMoveNumber+1, message = "Keep Going!" }, Cmd.none)

wrongMove : Model -> (Model, Cmd Msg)
wrongMove model = ({ model |  message = "Wrong!! You lose :( ", userMoveNumber = 0, sequence = Array.empty }, Cmd.none)


init : (Model, Cmd Msg)
init =
  (
    {
      sequence = Array.empty, 
      userMoveNumber=0, 
      message="", 
      buttons =
        [ { label = "0"
          , onClickAction = CheckMove 0
          , onMouseDownAction = AnimateActive 0
          , style = Animation.style [Animation.backgroundColor Color.red]
          , color = Color.red
          , colorString = "red"
          }
        , { label = "1"
          , onClickAction = CheckMove 1
          , onMouseDownAction = AnimateActive 1
          , style = Animation.style [Animation.backgroundColor Color.green]
          , color = Color.green
          , colorString = "green"
          }
        , { label = "2"
          , onClickAction = CheckMove 2
          , onMouseDownAction = AnimateActive 2
          , style = Animation.style [Animation.backgroundColor Color.blue]
          , color = Color.blue
          , colorString = "blue"
          }
        , { label = "3"
          , onClickAction = CheckMove 3
          , onMouseDownAction = AnimateActive 3
          , style = Animation.style [Animation.backgroundColor Color.yellow]
          , color = Color.yellow
          , colorString = "yellow"
          }
        ]
    }, 
    Random.generate NewValue (Random.int 0 3))


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <|
        List.map .style model.buttons

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick StartOver ] [ text "StartOver" ]
    , div [] [ text (toString model.sequence) ] 
    , div [] [ text ("Score: " ++ toString (Array.length model.sequence))]
    , div [] [ text ("Message: " ++ toString model.message)] 
    , div [] (List.map viewButton model.buttons)
    ]

viewButton : Button -> Html Msg
viewButton button =
    div
        (Animation.render button.style
            ++ [ style 
                  [ ( "backgroundColor", button.colorString )
                  , ("padding", "8px")
                  , ("display", "inline")]
              , Html.Events.onMouseDown  (button.onMouseDownAction)
              , onClick (button.onClickAction)
            ]
        )
        [ text button.label ]