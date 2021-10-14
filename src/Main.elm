module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Http
import Json.Decode exposing (Decoder, field, string, map5, keyValuePairs, map)



-- MAIN


main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }



-- MODEL


type alias PluginMeta =
    { name : String
    , description : String
    , author : String
    , license : String
    , hash : String
    }

type Plugin = 
    NoUrl PluginMeta
    | HasUrl (String, PluginMeta)

type Model =
    Loading
    | DisplayPlugins (List Plugin)
    | LoadError Http.Error


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getPluginList)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- UPDATE


type Msg
  = GotPlugins (Result Http.Error (List Plugin))
  | ReloadPlugins


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPlugins pluginRes ->
        case pluginRes of
            Ok plugins ->
                (DisplayPlugins plugins, Cmd.none)
            Err err ->
                (LoadError err, Cmd.none)
    ReloadPlugins ->
        (model, getPluginList)



-- VIEW


pluginToString : Plugin -> String
pluginToString p =
    case p of
        NoUrl m ->
            m.name
        HasUrl (_, m) ->
            m.name

view : Model -> Html Msg
view model =
  div []
    [
        case model of
            Loading ->
                div []
                [ text "Loading plugins, please wait..."
                ]

            DisplayPlugins plugins ->
                div [ style "margin" "1rem" ]
                    (plugins
                    |> List.map (\p ->
                        div [ style "margin-bottom" "0.5rem" ]
                        [ text (pluginToString p)
                        ]
                    ))

            LoadError err ->
                div []
                [ text "Loading plugins failed. Try again?"
                , button [ onClick ReloadPlugins ] [ text "Reload plugins" ]
                ]
    ]


-- HTTP


getPluginList : Cmd Msg
getPluginList =
    Http.get
    { url = "https://cumcordplugins.github.io/Condom/plugins-large.json"
    , expect = Http.expectJson GotPlugins pluginListDecoder
    }

pluginDecoder : Decoder PluginMeta
pluginDecoder =
    map5 PluginMeta
        (field "name" string)
        (field "description" string)
        (field "author" string)
        (field "license" string)
        (field "hash" string)

pluginListDecoder : Decoder (List Plugin)
pluginListDecoder =
    keyValuePairs pluginDecoder
        |> map (\l -> l |> List.map HasUrl)