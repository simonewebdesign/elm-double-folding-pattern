import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onFocus, onBlur, onWithOptions, targetValue)
import Signal exposing (Address)
import Keyboard
import Mouse
import Time
import String
import Json.Decode as JSON


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
  { cardNumber0: String
  , cardNumber1: String
  , cardNumber2: String
  , cardNumber3: String
  , cardHolderName: String
  , cardExpirationMonth: String
  , cardExpirationYear: String
  , cardCCV: String
  , cardCCVfocused: Bool
  , submitting: Bool
  }


initialViewState : ViewState
initialViewState =
  { cardNumber0 = ""
  , cardNumber1 = ""
  , cardNumber2 = ""
  , cardNumber3 = ""
  , cardHolderName = ""
  , cardExpirationMonth = ""
  , cardExpirationYear = ""
  , cardCCV = ""
  , cardCCVfocused = False
  , submitting = False
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
    , creditCardForm state
    , "state: " ++ (toString state) |> text
    , "model: " ++ (toString model) |> text
    ]


creditCardForm : ViewState -> Html
creditCardForm state =
  div [ class "checkout" ]
    [ div [ class ("credit-card-box" ++ if state.cardCCVfocused then " hover" else "") ]
      [ div [ class "flip" ]
        [ div [ class "front" ]
          [ div [ class "chip" ]
            []
          , div [ class "logo" ]
            [ img [ alt "", src "http://cdn.flaticon.com/svg/39/39134.svg" ]
              []
            , text "        "
            ]
          , div [ class "number" ]
            [ text (cardNumber state) ]
          , div [ class "card-holder" ]
            [ label []
              [ text "Card holder" ]
            , div []
              [ text state.cardHolderName ]
            ]
          , div [ class "card-expiration-date" ]
            [ label []
              [ text "Expires" ]
            , div []
              [ text (cardExpirationDate state) ]
            ]
          ]
        , div [ class "back" ]
          [ div [ class "strip" ]
            []
          , div [ class "logo" ]
            [ img [ alt "", src "http://cdn.flaticon.com/svg/39/39134.svg" ]
              []
            , text "        "
            ]
          , div [ class "ccv" ]
            [ label []
              [ text "CCV" ]
            , div []
              [ text state.cardCCV ]
            ]
          ]
        ]
      ]
    , Html.form [ class "form"
                , onWithOptions
                    "submit"
                    { preventDefault = True, stopPropagation = False }
                    (JSON.succeed Nothing)
                    (\_ -> Signal.message events.address Submit)
                ]
      [ fieldset [ class "card-number-inputs"
                 , disabled state.submitting
                 ]
        [ label [ for "card-number" ]
          [ text "Card Number" ]
        , input [ class "input-card-number"
                , id "card-number0"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry0 digit))
                ]
          []
        , text "      "
        , input [ class "input-card-number"
                , id "card-number1"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry1 digit))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber0 > 3
                ]
          []
        , text "      "
        , input [ class "input-card-number"
                , id "card-number2"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry2 digit))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber1 > 3
                ]
          []
        , text "      "
                , input [ class "input-card-number"
                , id "card-number3"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry3 digit))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber2 > 3
                ]
          []
        , text "    "
        ]
      , fieldset [ disabled state.submitting ]
        [ label [ for "card-holder" ]
          [ text "Card holder" ]
        , input [ id "card-holder"
                , type' "text"
                , on "input" targetValue (\entry -> Signal.message events.address (HolderEntry entry))
                ]
          []
        , text "    "
        ]
      , fieldset [ class "fieldset-expiration"
                 , disabled state.submitting
                 ]
        [ label [ for "card-expiration-month" ]
          [ text "Expiration date" ]
        , div [ class "select" ]
          [ select [ id "card-expiration-month"
                   , on "input" targetValue (\month -> Signal.message events.address (ExpirationMonthChange month))
                   ]
            [ option []
              []
            , option []
              [ text "Jan" ]
            , option []
              [ text "Feb" ]
            , option []
              [ text "Mar" ]
            , option []
              [ text "Apr" ]
            , option []
              [ text "May" ]
            , option []
              [ text "Jun" ]
            , option []
              [ text "Jul" ]
            , option []
              [ text "Ago" ]
            , option []
              [ text "Sep" ]
            , option []
              [ text "Oct" ]
            , option []
              [ text "Nov" ]
            , option []
              [ text "Dec" ]
            ]
          ]
        , div [ class "select" ]
          [ select [ id "card-expiration-year"
                   , on "input" targetValue (\year -> Signal.message events.address (ExpirationYearChange year))
                   ]
            [ option []
              []
            , option []
              [ text "2016" ]
            , option []
              [ text "2017" ]
            , option []
              [ text "2018" ]
            , option []
              [ text "2019" ]
            , option []
              [ text "2020" ]
            , option []
              [ text "2021" ]
            , option []
              [ text "2022" ]
            , option []
              [ text "2023" ]
            , option []
              [ text "2024" ]
            , option []
              [ text "2025" ]
            ]
          ]
        ]
      , fieldset [ class "fieldset-ccv"
                 , disabled state.submitting
                 ]
        [ label [ for "card-ccv" ]
          [ text "CCV" ]
        , input [ id "card-ccv"
                , maxlength 3
                , type' "text"
                , on "input" targetValue (\entry -> Signal.message events.address (CCVEntry entry))
                , onFocus events.address CCVFocused
                , onBlur events.address CCVFocusLeave
                ]
          []
        , text "    "
        ]
      , button [ class "btn"
               , disabled state.submitting
               ]
        [ i [ class "fa fa-cog fa-spin" ]
          []
        , text "submit"
        ]
      ]
    ]


cardNumber : ViewState -> String
cardNumber state =
  state.cardNumber0 ++ " " ++
  state.cardNumber1 ++ " " ++
  state.cardNumber2 ++ " " ++
  state.cardNumber3


cardExpirationMonth : ViewState -> String
cardExpirationMonth state =
  case state.cardExpirationMonth of
    "Jan" -> "01"
    "Feb" -> "02"
    "Mar" -> "03"
    "Apr" -> "04"
    "May" -> "05"
    "Jun" -> "06"
    "Jul" -> "07"
    "Aug" -> "08"
    "Sep" -> "09"
    "Oct" -> "10"
    "Nov" -> "11"
    "Dec" -> "12"
    _ -> ""


cardExpirationYear : ViewState -> String
cardExpirationYear state =
  String.right 2 state.cardExpirationYear


cardExpirationDate : ViewState -> String
cardExpirationDate state =
  (cardExpirationMonth state) ++ "/" ++ (cardExpirationYear state)


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
 | DigitEntry0 String
 | DigitEntry1 String
 | DigitEntry2 String
 | DigitEntry3 String
 | HolderEntry String
 | ExpirationMonthChange String
 | ExpirationYearChange String
 | CCVEntry String
 | CCVFocused
 | CCVFocusLeave
 | Submit


render : Event -> ViewState -> ViewState
render event state =
  case event of
    Never -> state
    DigitEntry0 newDigit -> { state | cardNumber0 = newDigit }
    DigitEntry1 newDigit -> { state | cardNumber1 = newDigit }
    DigitEntry2 newDigit -> { state | cardNumber2 = newDigit }
    DigitEntry3 newDigit -> { state | cardNumber3 = newDigit }
    HolderEntry newEntry -> { state | cardHolderName = newEntry }
    ExpirationMonthChange newMonth -> { state | cardExpirationMonth = newMonth }
    ExpirationYearChange  newYear ->  { state | cardExpirationYear = newYear }
    CCVEntry newEntry -> { state | cardCCV = newEntry }
    CCVFocused -> { state | cardCCVfocused = True }
    CCVFocusLeave -> { state | cardCCVfocused = False }
    Submit -> { state | submitting = True }
