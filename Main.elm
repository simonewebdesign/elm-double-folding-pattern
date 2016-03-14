import Html exposing (..)
import Html.Attributes exposing (..)
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
  { cardNumber0: String
  , cardNumber1: String
  , cardNumber2: String
  , cardNumber3: String
  , cardHolderName: String
  , cardExpirationMonth: String
  , cardExpirationYear: String
  , cardCCV: String
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
  div [ class "checkout" ]
    [ div [ class "credit-card-box" ]
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
            []
          , div [ class "card-holder" ]
            [ label []
              [ text "Card holder" ]
            , div []
              []
            ]
          , div [ class "card-expiration-date" ]
            [ label []
              [ text "Expires" ]
            , div []
              []
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
              []
            ]
          ]
        ]
      ]
    , Html.form [ autocomplete False, class "form", novalidate False ]
      [ fieldset []
        [ label [ for "card-number" ]
          [ text "Card Number" ]
        , input [ class "input-card-number"
                , id "card-number"
                , maxlength 4
                , type' "num"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry0 digit))
                ]
          []
        , text "      "
        , input [ class "input-card-number"
                , id "card-number"
                , maxlength 4
                , type' "num"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry1 digit))
                ]
          []
        , text "      "
        , input [ class "input-card-number"
                , id "card-number"
                , maxlength 4
                , type' "num"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry2 digit))
                ]
          []
        , text "      "
                , input [ class "input-card-number"
                , id "card-number"
                , maxlength 4
                , type' "num"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry3 digit))
                ]
          []
        , text "    "
        ]
      , fieldset []
        [ label [ for "card-holder" ]
          [ text "Card holder" ]
        , input [ id "card-holder", type' "text" ]
          []
        , text "    "
        ]
      , fieldset [ class "fieldset-expiration" ]
        [ label [ for "card-expiration-month" ]
          [ text "Expiration date" ]
        , div [ class "select" ]
          [ select [ id "card-expiration-month" ]
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
          [ select [ id "card-expiration-year" ]
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
      , fieldset [ class "fieldset-ccv" ]
        [ label [ for "card-ccv" ]
          [ text "CCV" ]
        , input [ id "card-ccv", maxlength 3, type' "text" ]
          []
        , text "    "
        ]
      , button [ class "btn" ]
        [ i [ class "fa fa-lock" ]
          []
        , text "submit"
        ]
      ]
    ]


creditCardInput : Html
creditCardInput =
  input
    [ type' "text"
    --, on "input" targetValue (\digit -> Signal.message events.address (DigitEntry digit))
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
 | DigitEntry0 String
 | DigitEntry1 String
 | DigitEntry2 String
 | DigitEntry3 String


render : Event -> ViewState -> ViewState
render event state =
  case event of
    Never -> state
    DigitEntry0 newDigit -> { state | cardNumber0 = newDigit }
    DigitEntry1 newDigit -> { state | cardNumber1 = newDigit }
    DigitEntry2 newDigit -> { state | cardNumber2 = newDigit }
    DigitEntry3 newDigit -> { state | cardNumber3 = newDigit }
