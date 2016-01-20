import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Signal, Address)
import StartApp.Simple as StartApp


-- Use StartApp to boot up the application.
main =
    StartApp.start { model = model, view = view, update = update }


-- Our model contains the list of todo items, and the current value of the
-- input field.
type alias Model =
    { items : List String,
      currentValue : String
    }


-- Our initial model of the list of items we have to do.
model : Model
model = 
    { currentValue = "",
      items =  [ "Drink beer"
               , "Eat hamburgers"
               , "..."
               , "Profit!"
               ]
    }


-- Renders one string into a nice list item.
item : Address Action -> String -> Html
item address str =
    li [] 
       [ text str
       , button [ onClick address (Remove str) ] [ text "x" ]
       ]


-- Widget for changing the current value we want to add.
currentValueInput : Address Action -> String -> Html
currentValueInput address str =
    input [ type' "text"
          , value str
          , on "input" targetValue (Signal.message address << UpdateCurrentValue)
          ]
          []


-- Button which, when clicked, adds the current value to the list of items.
addCurrentValue address str =
    button [ onClick address (Add str) ]
           [ text "add" ]
    

-- Stylesheets for the main container element.
pageStyle : Html.Attribute
pageStyle = 
    style 
       [ ("backgroundColor", "grey")
       , ("color", "white")
       , ("padding", "20px")
       ]


-- Renders the list of items and a header.
view : Address Action -> Model -> Html
view address model = 
    div [ pageStyle ] 
        [ h1 [] [ text "Important items to do" ]
        , div [] [ ul [] (List.map (item address) model.items) ]
        , currentValueInput address model.currentValue
        , addCurrentValue address model.currentValue
        ]


type Action = 
    NoOp
    | Add String
    | Remove String
    | UpdateCurrentValue String


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Add str ->
            { model | items = str :: model.items, currentValue = "" }

        Remove str -> 
            { model | items = List.filter ((/=) str) model.items }

        UpdateCurrentValue str ->
            { model | currentValue = str }
