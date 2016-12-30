import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onMouseDown)
import Html.Attributes exposing (style)
import Random 
import Array
import Animation 
import Color
import Task
import Time exposing (second)


main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Button =
  { label : String --for debugging purposes
  , onClickAction : Msg
  , onMouseDownAction : Msg
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
  { 
    sequence  : Array.Array Int,
    userMoveNumber : Int,
    message : String,
    buttons : List Button
  }

-- ref: http://folkertdev.nl/blog/task-perform-with-task-succeed/
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
        updatedModel = {model | sequence = Array.push newValue model.sequence}
      in
        update (Play 0) updatedModel

    Play index ->
      case Array.get index model.sequence of 
          Just buttonToAnimate -> 
            let 
              (newModel, cmd) = (update (AnimateActive buttonToAnimate index) model)
            in 
              if (reachedEndOfSequence index model.sequence) then
                ({ newModel | message = "Playing " ++ (toString buttonToAnimate)}, Cmd.none)
              else
                andThen ({ newModel | message = "Playing " ++ (toString buttonToAnimate)}, cmd) (update (Play (index+1)))
          Nothing -> 
            ({model| message = "Unexpected error: index out of range in Play function"}, Cmd.none)
        
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

    AnimateActive indexButton delay ->
      let currentButtonColor = Array.get indexButton (Array.fromList model.buttons)
        |> Maybe.map (\button -> button.color)
        |> Maybe.withDefault Color.white

      in 
      ( onButtonStyle model indexButton <|
                (Animation.queue
                    [ 
                    Animation.wait (toFloat delay * 1 * second)
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
  (index+1 == Array.length sequence)

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


goodMove : Model -> (Model, Cmd Msg)
goodMove model =
  if (reachedEndOfSequence model.userMoveNumber model.sequence) then
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
          , onMouseDownAction = AnimateActive 0 0
          , style = Animation.style [Animation.backgroundColor Color.red]
          , color = Color.red
          }
        , { label = "1"
          , onClickAction = CheckMove 1
          , onMouseDownAction = AnimateActive 1 0 
          , style = Animation.style [Animation.backgroundColor Color.green]
          , color = Color.green
          }
        , { label = "2"
          , onClickAction = CheckMove 2
          , onMouseDownAction = AnimateActive 2 0
          , style = Animation.style [Animation.backgroundColor Color.blue]
          , color = Color.blue
          }
        , { label = "3"
          , onClickAction = CheckMove 3
          , onMouseDownAction = AnimateActive 3 0
          , style = Animation.style [Animation.backgroundColor Color.yellow]
          , color = Color.yellow
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
  div [style [
    ("position", "absolute")
    , ("top", "0")
    , ("bottom", "0")
    , ("left", "0")
    , ("right", "0")
    , ("margin", "auto")
    , ("width", "500px")
    , ("height", "500px")
  ]]
    [ 
     div [] [ text ("Score: " ++ toString ((Array.length model.sequence)-1))]
    , div [] [ text ("Message: " ++ toString model.message)] 
    , div [ style [
    ("width", "250px")
    , ("height", "250px")
    , ("border-radius", "125px")
    , ("background", "black")
    ]] 
    [ 
      div [] [
        div [] (List.map viewButton (List.take 2 model.buttons))
        , div [] (List.map viewButton (List.drop 2 model.buttons))
      ] 
    ]
    , button [ onClick StartOver ] [ text "StartOver" ]
    ]


viewButton : Button -> Html Msg
viewButton button =
    div
        (Animation.render button.style
            ++ [ style 
                  [ ("padding", "8px")
                  , ("width", "40px")
                  , ("height", "40px")
                  , ("border-radius", "10px")
                  , ("display", "inline-block")]

              , Html.Events.onMouseDown (button.onMouseDownAction)
              , onClick (button.onClickAction)
            ]
        )
        [ ]