module Main exposing (..)

import Dict
import Html exposing (Html, div, option, select, small, text)
import Html.Attributes exposing (class, selected)
import Html.Events exposing (onInput)
import List.Extra exposing (uniqueBy)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Country =
    { name : String }


type alias City =
    { country : Country
    , name : String
    , price : Price
    }


type alias Price =
    Int


type Selected
    = CitySelected City
    | CountrySelected Country
    | NothingSelected


type alias Model =
    { selected : Selected }


cities : List City
cities =
    [ { name = "Melbourne"
      , country = { name = "Australia" }
      , price = 100
      }
    , { name = "Sydney"
      , country = { name = "Australia" }
      , price = 110
      }
    , { name = "London"
      , country = { name = "England" }
      , price = 1000
      }
    , { name = "Manchester"
      , country = { name = "England" }
      , price = 1200
      }
    , { name = "Paris"
      , country = { name = "France" }
      , price = 900
      }
    , { name = "Lille"
      , country = { name = "France" }
      , price = 800
      }
    ]


countries : List City -> List Country
countries cities =
    cities
        |> List.map .country
        |> uniqueBy toString



-- MODEL


model : Model
model =
    { selected = NothingSelected }



-- UPDATE


type Msg
    = SelectCountry String
    | SelectCity String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCountry country ->
            { model | selected = setCountry country }

        SelectCity city ->
            { model | selected = setCity city }


setCountry : String -> Selected
setCountry string =
    cities
        |> countries
        |> List.filter (.name >> (==) string)
        |> List.head
        |> Maybe.map CountrySelected
        |> Maybe.withDefault NothingSelected


setCity : String -> Selected
setCity string =
    cities
        |> List.filter (.name >> (==) string)
        |> List.head
        |> Maybe.map CitySelected
        |> Maybe.withDefault NothingSelected



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "form-group" ]
        [ select [ class "form-control", onInput SelectCountry ] <|
            renderOption False "Select Country"
                :: countryOptions model
        , select [ class "form-control", onInput SelectCity ] <|
            renderOption False "Select City"
                :: cityOptions model
        , renderPrice model
        ]


countryOptions : Model -> List (Html Msg)
countryOptions { selected } =
    List.map (renderCountry selected) <| countries cities


cityOptions : Model -> List (Html Msg)
cityOptions model =
    model |> activeCities |> List.map (renderCity model.selected)


activeCities : Model -> List City
activeCities { selected } =
    case selected of
        CitySelected selectedCity ->
            citiesIn cities selectedCity.country

        CountrySelected selectedCountry ->
            citiesIn cities selectedCountry

        NothingSelected ->
            cities


citiesIn : List City -> Country -> List City
citiesIn cities country =
    List.filter (.country >> (==) country) cities


renderCountry : Selected -> Country -> Html Msg
renderCountry selected country =
    let
        isSelected =
            case selected of
                CitySelected selectedCity ->
                    country == selectedCity.country

                CountrySelected selectedCountry ->
                    country == selectedCountry

                NothingSelected ->
                    False
    in
        renderOption isSelected country.name


renderCity : Selected -> City -> Html Msg
renderCity selected city =
    let
        isSelected =
            case selected of
                CitySelected selectedCity ->
                    city == selectedCity

                _ ->
                    False
    in
        renderOption isSelected city.name


renderOption : Bool -> String -> Html Msg
renderOption isSelected value =
    option [ selected isSelected ] [ text value ]


renderPrice : Model -> Html Msg
renderPrice { selected } =
    case selected of
        CitySelected selectedCity ->
            div [ class "price" ]
                [ small [] [ text "from" ]
                , text <| " $" ++ toString selectedCity.price
                ]

        _ ->
            text ""
