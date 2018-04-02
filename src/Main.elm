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


type Country
    = Australia
    | England
    | France


type Selected
    = CitySelected City
    | CountrySelected Country
    | None


type alias City =
    { country : Country
    , name : String
    , price : Int
    }


type alias Model =
    { selected : Selected }


cities =
    [ { name = "Melbourne"
      , country = Australia
      , price = 100
      }
    , { name = "Sydney"
      , country = Australia
      , price = 110
      }
    , { name = "London"
      , country = England
      , price = 1000
      }
    , { name = "Manchester"
      , country = England
      , price = 1200
      }
    , { name = "Paris"
      , country = France
      , price = 900
      }
    , { name = "Lille"
      , country = France
      , price = 800
      }
    ]



-- MODEL


model : Model
model =
    { selected = None }



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
    case string of
        "Australia" ->
            CountrySelected Australia

        "England" ->
            CountrySelected England

        "France" ->
            CountrySelected France

        _ ->
            None


setCity : String -> Selected
setCity string =
    cities
        |> List.filter (.name >> (==) string)
        |> List.head
        |> Maybe.map CitySelected
        |> Maybe.withDefault None



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
                [ renderOption False "Select Country"
                , renderCountry model.selected Australia
                , renderCountry model.selected England
                , renderCountry model.selected France
                ]
            ]
        , div []
            [ select
                [ class "form-control"
                , onInput SelectCity
                ]
              <|
                [ renderOption False "Select City"
                ]
                    ++ (activeCities model |> List.map (renderCity model.selected))
            ]
        , renderPrice model
        ]


countryName : Country -> String
countryName country =
    case country of
        Australia ->
            "Australia"

        England ->
            "England"

        France ->
            "France"


activeCities : Model -> List City
activeCities { selected } =
    case selected of
        CitySelected selectedCity ->
            List.filter (.country >> (==) selectedCity.country) cities

        CountrySelected selectedCountry ->
            List.filter (.country >> (==) selectedCountry) cities

        None ->
            cities


renderCountry : Selected -> Country -> Html Msg
renderCountry selected country =
    let
        isSelected =
            case selected of
                CitySelected selectedCity ->
                    country == selectedCity.country

                CountrySelected selectedCountry ->
                    country == selectedCountry

                None ->
                    False
    in
        renderOption isSelected <| countryName country


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
                [ small [] [ text "from $" ]
                , text <| toString selectedCity.price
                ]

        _ ->
            text ""
