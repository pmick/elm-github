module Main exposing (..)

import Html exposing (Html, li, ul, div, text, input, program)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Http exposing (send, get)
import Json.Decode exposing (Decoder, map3, at, list, field, string, int)

-- MODEL


type alias Model =
    { repositories: (List Repository)
    }

type alias RepositoryId = Int

type alias Repository =
    { id: RepositoryId
    , title: String
    , stars: Int
    }


init : ( Model, Cmd Msg )
init =
    ( { repositories = [ ] }, Cmd.none )


type Route
    = RepositoriesRoute
    | RepositoryRoute RepositoryId
    | NotFoundRoute


-- MESSAGES


type Msg
    = SearchTermUpdated String
    | NewRepositories (Result Http.Error ( List Repository ))



-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
        input [ placeholder "Repository Name", onInput SearchTermUpdated ] [],
        repositoriesListView model.repositories
    ]


repositoriesListView: List Repository -> Html Msg
repositoriesListView repositories =
    repositories
        |> List.map repositoryView
        |> ul []


repositoryView: Repository -> Html Msg
repositoryView repository =
    li [] [ text repository.title ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchTermUpdated searchTerm ->
            ( model, getRepositoryListData searchTerm )
        NewRepositories  (Ok newRepositories) ->
            ( Model newRepositories, Cmd.none )
        NewRepositories (Err _) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- LIB


getRepositoryListData : String -> Cmd Msg
getRepositoryListData searchTerm =
    send NewRepositories 
        (get ( makeSearchUrl searchTerm ) decodeRepositories)


makeSearchUrl : String -> String
makeSearchUrl searchTerm =
    "https://api.github.com/search/repositories?q=" ++ searchTerm ++ "&sort=stars&order=desc"


decodeRepositories : Decoder (List Repository)
decodeRepositories = 
    at ["items"]
        (list decodeRepository)


decodeRepository : Decoder Repository
decodeRepository =
    map3 Repository
        (field "id" int)
        (field "full_name" string)
        (field "stargazers_count" int)

