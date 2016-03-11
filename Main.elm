import Html exposing (Html, div, button, text, input, form)
import Html.Attributes exposing (type')
import Html.Events exposing (onClick)
import Signal exposing (Address)
import Keyboard
import Mouse
import Time


type alias Model =
  { counter: Int
  }


initialModel : Model
initialModel =
  { counter = 0
  }


model : Signal Model
model =
  Signal.foldp update initialModel inputs


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


inputs : Signal Action
inputs =
  let
    x = Signal.map .x Keyboard.arrows
    delta = Time.fps 30
    toAction n =
      case n of
        -1 -> Decrement
        1 -> Increment
        _ -> NoOp

    arrows = Signal.sampleOn delta (Signal.map toAction x)
    clicks = Signal.map (always Increment) Mouse.clicks
  in
    Signal.mergeMany [actions.signal, arrows, clicks]


main : Signal Html
main =
  Signal.map (view actions.address) model


-- VIEWS


view : Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model.counter) ]
    , button [ onClick address Increment ] [ text "+" ]
    , creditCardForm
    ]


creditCardForm : Html
creditCardForm =
  form
    []
    [ creditCardInput
    , submitButton
    ]


creditCardInput : Html
creditCardInput =
  input
    [ type' "text"
    ] []


submitButton : Html
submitButton =
  input
    [ type' "submit"
    ] []


type Action
  = NoOp
  | Increment
  | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Increment -> { model | counter = model.counter + 1 }
    Decrement -> { model | counter = model.counter - 1 }
