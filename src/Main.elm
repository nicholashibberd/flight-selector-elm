module Main exposing (..)

import Dict
import Html exposing (Html, div, option, select, small, text)
import Html.Attributes exposing (class, selected)
import Html.Events exposing (onInput)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Country =
    { name : String
    , cities : List City
    }


type alias City =
    { name : String
    , price : Int
    }


type alias Model =
    { selectedCountry : Maybe Country
    , selectedCity : Maybe City
    , price : Maybe Int
    }



-- MODEL


model : Model
model =
    { selectedCountry = Nothing
    , selectedCity = Nothing
    , price = Nothing
    }


countries : List Country
countries =
    [ { name = "Australia"
      , cities =
            [ { name = "Melbourne", price = 100 }
            , { name = "Sydney", price = 110 }
            ]
      }
    , { name = "England"
      , cities =
            [ { name = "London", price = 1000 }
            , { name = "Manchester", price = 1200 }
            ]
      }
    , { name = "France"
      , cities =
            [ { name = "Paris", price = 900 }
            , { name = "Lille", price = 800 }
            ]
      }
    ]



-- UPDATE


type Msg
    = SelectCountry String
    | SelectCity String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCountry country ->
            { model
                | selectedCountry = setCountry country model
                , selectedCity = Nothing
                , price = Nothing
            }

        SelectCity city ->
            { model
                | selectedCity = setCity city model
                , price = setPrice city
            }


setCountry : String -> Model -> Maybe Country
setCountry string model =
    find (\country -> country.name == string) countries


setCity : String -> Model -> Maybe City
setCity string model =
    find (\city -> city.name == string) <| cities


cities : List City
cities =
    List.concatMap .cities countries


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


setPrice : String -> Maybe Int
setPrice city =
    let
        dict =
            Dict.fromList
                [ ( "Melbourne", 100 )
                , ( "Sydney", 110 )
                , ( "London", 1000 )
                , ( "Manchester", 1200 )
                , ( "Paris", 900 )
                , ( "Lille", 800 )
                ]
    in
        Dict.get city dict



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "form-group" ]
            [ select
                [ class "form-control"
                , onInput SelectCountry
                ]
              <|
                [ renderOption True "Select Country"
                ]
                    ++ countryOptions model
            ]
        , div []
            [ select
                [ class "form-control"
                , onInput SelectCity
                ]
              <|
                [ renderOption True "Select City"
                ]
                    ++ (cityOptions model)
            ]
        , renderPrice model
        ]


countryOptions : Model -> List (Html Msg)
countryOptions { selectedCountry } =
    List.map (renderCountry selectedCountry) countries


renderCountry : Maybe Country -> Country -> Html Msg
renderCountry selectedCountry country =
    renderOption (isEqual selectedCountry country) country.name


cityOptions : Model -> List (Html Msg)
cityOptions { selectedCountry, selectedCity } =
    case selectedCountry of
        Nothing ->
            []

        Just country ->
            List.map (renderCity selectedCity) country.cities


renderCity : Maybe City -> City -> Html Msg
renderCity selectedCity city =
    renderOption (isEqual selectedCity city) city.name


renderPrice : Model -> Html Msg
renderPrice { price } =
    case price of
        Nothing ->
            text ""

        Just val ->
            div [ class "price" ]
                [ small [] [ text "from $" ]
                , text <| toString val
                ]


renderOption : Bool -> String -> Html Msg
renderOption isSelected value =
    option [ selected isSelected ] [ text value ]


isEqual : Maybe a -> a -> Bool
isEqual maybe a =
    maybe
        |> Maybe.map ((==) a)
        |> Maybe.withDefault False
