module Main exposing (main)

import PixelEngine exposing (PixelEngine, Area, Input(..), game)
import PixelEngine.Options as Options exposing (Options)

import Grid.Position exposing (Position)
import Grid.Direction exposing (Direction(..))

import GameState as GameState exposing (GameState)
import WorldTile exposing (WorldTile)


type Model
    = InGame GameState


type Msg
    = GameStateMsg GameState.Msg
    | Move Direction
    -- | ChangeInteractionMode
    -- | ChangeUiFocus
    | SecondaryButton1
    | SecondaryButton2
    | TileClick (Position, WorldTile)


main : PixelEngine () Model Msg
main = game 
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , controls = controls
    , width = 170
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (gameState, cmd) = GameState.init
    in
    ( InGame gameState
    , Cmd.map GameStateMsg cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (InGame gameState) =
    let
        gameStateMsgM = case msg of
            Move direction -> Just (GameState.Move direction)
            GameStateMsg gameStateMsg -> Just gameStateMsg
            SecondaryButton1 -> Just GameState.SecondaryButton1
            SecondaryButton2 -> Just GameState.SecondaryButton2
            TileClick event -> Just <| GameState.TileClick event

        (newGameState, cmd)
            = gameStateMsgM
            |> Maybe.map (\x -> GameState.updateGame x gameState)
            |> Maybe.withDefault (gameState, Cmd.none)
    in
    ( InGame newGameState, Cmd.map GameStateMsg cmd )


areas : Model -> List (Area Msg)
areas (InGame gameState) =
    List.map (PixelEngine.mapArea GameStateMsg) (GameState.areas gameState)


options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.1


-- type Scene =
--     Scene Dict Position (Tile Msg)


view :
    Model -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Farm"
    , options = Just options
    , body = areas model
    }


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Move Up

        InputDown ->
            Just <| Move Down

        InputLeft ->
            Just <| Move Left

        InputRight ->
            Just <| Move Right

        InputX ->
            Just <| SecondaryButton1

        InputY ->
            Just <| SecondaryButton2

        _ ->
            Nothing