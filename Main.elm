import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onDoubleClick, onFocus, onBlur, onWithOptions, targetValue)
import Signal exposing (Address)
import Char exposing (isDigit)
import String
import Json.Decode as JSON exposing ((:=))
import Http
import Task exposing (Task, andThen, onError)


type alias Model =
  { creditCard: CreditCard
  }


type alias CreditCard =
  { number: String
  , holder: String
  , expiration: String
  , ccv: String
  }


initialModel : Model
initialModel =
  { creditCard = CreditCard "" "" "" ""
  }


type alias ViewState =
  { activeView: ActiveView
  , cardNumber0: String
  , cardNumber1: String
  , cardNumber2: String
  , cardNumber3: String
  , cardHolderName: String
  , cardExpirationMonth: String
  , cardExpirationYear: String
  , cardCCV: String
  , cardCCVfocused: Bool
  , submitting: Bool
  , debug: Bool
  }


initialViewState : ViewState
initialViewState =
  { activeView = CreditCardForm
  , cardNumber0 = ""
  , cardNumber1 = ""
  , cardNumber2 = ""
  , cardNumber3 = ""
  , cardHolderName = ""
  , cardExpirationMonth = ""
  , cardExpirationYear = ""
  , cardCCV = ""
  , cardCCVfocused = False
  , submitting = False
  , debug = False
  }


type ActiveView = CreditCardForm | Success | Fail Http.Error


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
  actions.signal


main : Signal Html
main =
  Signal.map2 view viewState model


-- VIEWS


view : ViewState -> Model -> Html
view state model =
  div []
    [ (activeView state.activeView) state model
    , debugView state model
    ]


debugView : ViewState -> Model -> Html
debugView state model =
  div [ class ("debug" ++ if state.debug then " active" else "")
      , onDoubleClick events.address ToggleDebug
      ]
      [ "state: " ++ (toString state) |> text
      , br [] []
      , "model: " ++ (toString model) |> text
      ]


activeView : ActiveView -> (ViewState -> Model -> Html)
activeView view =
  case view of
    CreditCardForm -> creditCardForm
    Success -> successView
    Fail error -> failView error


creditCardForm : ViewState -> Model -> Html
creditCardForm state model =
  div [ class "checkout" ]
    [ div [ class ("credit-card-box" ++ if state.cardCCVfocused then " hover" else "") ]
      [ div [ class "flip" ]
        [ div [ class "front" ]
          [ div [ class "chip" ]
            []
          , div [ class "logo" ]
            [ img [ alt "", src "http://cdn.flaticon.com/svg/39/39134.svg" ]
              []
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
                    (\_ -> Signal.message tasksMailbox.address (submit state))
                ]
      [ fieldset [ class "card-number-inputs"
                 , disabled state.submitting
                 ]
        [ label [ for "card-number" ]
          [ text "Card Number" ]
        , input [ class "input-card-number"
                , id "card-number0"
                , required True
                , pattern "\\d{4}"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry0 (String.filter isDigit digit)))
                , value state.cardNumber0
                ]
          []
        , input [ class "input-card-number"
                , id "card-number1"
                , required True
                , pattern "\\d{4}"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry1 (String.filter isDigit digit)))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber0 > 3
                , value state.cardNumber1
                ]
          []
        , input [ class "input-card-number"
                , id "card-number2"
                , required True
                , pattern "\\d{4}"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry2 (String.filter isDigit digit)))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber1 > 3
                , value state.cardNumber2
                ]
          []
        , input [ class "input-card-number"
                , id "card-number3"
                , required True
                , pattern "\\d{4}"
                , maxlength 4
                , type' "text"
                , on "input" targetValue (\digit -> Signal.message events.address (DigitEntry3 (String.filter isDigit digit)))
                , attribute "data-autofocus" <| toString <| String.length state.cardNumber2 > 3
                , value state.cardNumber3
                ]
          []
        ]
      , fieldset [ disabled state.submitting ]
        [ label [ for "card-holder" ]
          [ text "Card holder" ]
        , input [ id "card-holder"
                , required True
                , pattern "\\D+"
                , maxlength 50
                , type' "text"
                , on "input" targetValue (\entry -> Signal.message events.address (HolderEntry entry))
                , value state.cardHolderName
                ]
          []
        ]
      , fieldset [ class "fieldset-expiration"
                 , disabled state.submitting
                 ]
        [ label [ for "card-expiration-month" ]
          [ text "Expiration date" ]
        , div [ class "select" ]
          [ select [ id "card-expiration-month"
                   , required True
                   , on "input" targetValue (\month -> Signal.message events.address (ExpirationMonthChange month))
                   , value state.cardExpirationMonth
                   ]
            [ option []
              []
            , option [ selected <| state.cardExpirationMonth == "Jan" ]
              [ text "Jan" ]
            , option [ selected <| state.cardExpirationMonth == "Feb" ]
              [ text "Feb" ]
            , option [ selected <| state.cardExpirationMonth == "Mar" ]
              [ text "Mar" ]
            , option [ selected <| state.cardExpirationMonth == "Apr" ]
              [ text "Apr" ]
            , option [ selected <| state.cardExpirationMonth == "May" ]
              [ text "May" ]
            , option [ selected <| state.cardExpirationMonth == "Jun" ]
              [ text "Jun" ]
            , option [ selected <| state.cardExpirationMonth == "Jul" ]
              [ text "Jul" ]
            , option [ selected <| state.cardExpirationMonth == "Ago" ]
              [ text "Ago" ]
            , option [ selected <| state.cardExpirationMonth == "Sep" ]
              [ text "Sep" ]
            , option [ selected <| state.cardExpirationMonth == "Oct" ]
              [ text "Oct" ]
            , option [ selected <| state.cardExpirationMonth == "Nov" ]
              [ text "Nov" ]
            , option [ selected <| state.cardExpirationMonth == "Dec" ]
              [ text "Dec" ]
            ]
          ]
        , div [ class "select" ]
          [ select [ id "card-expiration-year"
                   , required True
                   , on "input" targetValue (\year -> Signal.message events.address (ExpirationYearChange year))
                   , value state.cardExpirationYear
                   ]
            [ option []
              []
            , option [ selected <| state.cardExpirationYear == "2016" ]
              [ text "2016" ]
            , option [ selected <| state.cardExpirationYear == "2017" ]
              [ text "2017" ]
            , option [ selected <| state.cardExpirationYear == "2018" ]
              [ text "2018" ]
            , option [ selected <| state.cardExpirationYear == "2019" ]
              [ text "2019" ]
            , option [ selected <| state.cardExpirationYear == "2020" ]
              [ text "2020" ]
            , option [ selected <| state.cardExpirationYear == "2021" ]
              [ text "2021" ]
            , option [ selected <| state.cardExpirationYear == "2022" ]
              [ text "2022" ]
            , option [ selected <| state.cardExpirationYear == "2023" ]
              [ text "2023" ]
            , option [ selected <| state.cardExpirationYear == "2024" ]
              [ text "2024" ]
            , option [ selected <| state.cardExpirationYear == "2025" ]
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
                , required True
                , maxlength 3
                , type' "text"
                , on "input" targetValue (\entry -> Signal.message events.address (CCVEntry (String.filter isDigit entry)))
                , onFocus events.address ToggleCCVFocus
                , onBlur events.address ToggleCCVFocus
                , value state.cardCCV
                ]
          []
        ]
      , button [ class "btn"
               , disabled state.submitting
               ]
        [ if state.submitting then i [ class "fa fa-cog fa-spin" ] [] else text ""
        , text "Submit"
        ]
      ]
    ]


successView : ViewState -> Model -> Html
successView state model =
  div [ class "checkout modalbox success center animate" ]
  [ div [ class "icon" ]
    [ i [ class "fa fa-check" ]
      []
    ]
  , h1 []
    [ text "Success!" ]
  , p []
    [ text "We've sent a confirmation to your e-mail." ]
  , h2 []
    [ text "Payment details" ]
  , dl []
    [ dt []
      [ text "Card number" ]
    , dd []
      [ text model.creditCard.number ]
    , dt []
      [ text "Card holder" ]
    , dd []
      [ text model.creditCard.holder ]
    ]
  ]


failView : Http.Error -> ViewState -> Model -> Html
failView error state model =
  div [ class "checkout modalbox error center animate" ]
    [ div [ class "icon" ]
      [ i [ class "fa fa-times" ]
        []
      ]
    , h1 []
      [ text "Oh no!" ]
    , p []
      [ text "Something went wrong, please try again." ]
    , button [ class "redo btn"
             , type' "button"
             , onClick events.address (ChangeView CreditCardForm)
             ]      [ text "Go back" ]
    , span [ class "change" ]
      [ text <| "Error type: " ++ (toString error) ]
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
  | CardSubmitted CreditCard


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    CardSubmitted newCard -> { model | creditCard = newCard }


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
  | ToggleCCVFocus
  | ToggleSubmit
  | ToggleDebug
  | ChangeView ActiveView


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
    ToggleCCVFocus -> { state | cardCCVfocused = not state.cardCCVfocused }
    ToggleSubmit -> { state | submitting = not state.submitting }
    ToggleDebug -> { state | debug = not state.debug }
    ChangeView newView -> { state | activeView = newView }


type alias Payload = { card: CreditCard }


postForm : ViewState -> Task Http.Error Payload
postForm state =
  let
    creditCard = CreditCard
      (cardNumber state)
      state.cardHolderName
      (cardExpirationDate state)
      state.cardCCV
    url = "https://elm.herokuapp.com/api"
    body =
      Http.multipart
        [ Http.stringData "card[number]" creditCard.number
        , Http.stringData "card[holder]" creditCard.holder
        , Http.stringData "card[expiration]" creditCard.expiration
        , Http.stringData "card[ccv]" creditCard.ccv
        ]
  in
    Http.post responseDecoder url body


responseDecoder : JSON.Decoder Payload
responseDecoder =
  JSON.object1 Payload
    ("card" := creditCardDecoder)


creditCardDecoder : JSON.Decoder CreditCard
creditCardDecoder =
  JSON.object4 CreditCard
    ("number" := JSON.string)
    ("holder" := JSON.string)
    ("expiration" := JSON.string)
    ("ccv" := JSON.string)


submit : ViewState -> Task x ()
submit state =
  toggleSubmit
  `andThen` (\_ -> postForm state)
  `andThen` (\{card} -> updateCardDetails card)
  `andThen` (\_ -> changeViewTo Success)
  `onError` (\err ->
    changeViewTo (Fail err)
    `andThen` (\_ -> toggleSubmit))


toggleSubmit : Task x ()
toggleSubmit =
  Signal.send events.address ToggleSubmit


updateCardDetails : CreditCard -> Task x ()
updateCardDetails card =
  Signal.send actions.address (CardSubmitted card)


changeViewTo : ActiveView -> Task x ()
changeViewTo newView =
  Signal.send events.address (ChangeView newView)


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal
