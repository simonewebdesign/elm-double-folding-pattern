import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

type alias Model =
  { counter: Int
  }


initialModel : Model
initialModel =
  { counter = 0
  }


main : Signal Html
main =
  StartApp.start { model = initialModel, view = view, update = update }


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model.counter) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


type Action = Increment | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    Increment -> { model | counter = model.counter + 1 }
    Decrement -> { model | counter = model.counter - 1 }
