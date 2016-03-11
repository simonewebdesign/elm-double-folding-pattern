import Html exposing (Html, div, button, text, input, form)
import Html.Attributes exposing (type')
import Html.Events exposing (on, onClick, targetValue)
import Signal exposing (Address)
import Keyboard
import Mouse
import Time


type alias Model =
  { counter: Int
  , cardNumber: String
  }


initialModel : Model
initialModel =
  { counter = 0
  , cardNumber = ""
  }


type alias ViewState =
  { digits: String
  }


initialViewState : ViewState
initialViewState =
  { digits = ""
  }


model : Signal Model
model =
  Signal.foldp update initialModel inputs


viewState : Signal ViewState
viewState =
  Signal.foldp render initialViewState events.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


events : Signal.Mailbox Event
events =
  Signal.mailbox Never


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
  Signal.map2 (view actions.address) viewState model


-- VIEWS


view : Address Action -> ViewState -> Model -> Html
view address state model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model.counter) ]
    , button [ onClick address Increment ] [ text "+" ]
    , creditCardForm
    , "state: " ++ (toString state) |> text
    , "model: " ++ (toString model) |> text
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
    , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry digit))
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
  | CompletedCardNumber String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Increment -> { model | counter = model.counter + 1 }
    Decrement -> { model | counter = model.counter - 1 }
    CompletedCardNumber newNumber ->
      { model | cardNumber = newNumber }


type Event
 = Never
 | DigitEntry String


render : Event -> ViewState -> ViewState
render event state =
  case event of
    Never -> state
    DigitEntry newDigit -> { state | digits = state.digits ++ newDigit }
