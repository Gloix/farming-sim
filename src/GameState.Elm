module GameState exposing (GameState, Msg(..), areas, init, updateGame)

import Array as Array exposing (Array)
import Bytes exposing (Bytes)
import Color
import Dict as Dict exposing (Dict)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import Html.Attributes exposing (style)
import Http
import Image as Image exposing (Image)
import PixelEngine exposing (Area, Input(..))
import PixelEngine.Tile as Tile exposing (Tile)
import Player as Player exposing (Player)
import Random
import WorldTile as WorldTile exposing (WorldTile)


maxInt : Int
maxInt =
    4294967296


playerColor : Int
playerColor =
    0xFF00FFFF


rows : Int
rows =
    11


manhattanDistance : Position -> Position -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x2 - x1) + abs (y2 - y1)



-- type InteractionMode
--     = Playing
--     | Editing UiFocus


type alias ActionBar =
    { items : Array (Maybe WorldTile)
    , selected : Int
    }


type UiFocus
    = CharacterFocus
    | ActionBarFocus



-- type Item
--     = WorldTile
--     | Entity
-- type Entity
--     =


type alias GameState =
    { mainPlayer : Player
    , otherPlayers : Dict Player.PlayerId Player
    , map : Dict Position WorldTile
    , cameraCenterPos : Position

    -- , interactionMode : InteractionMode
    , actionBar : ActionBar
    }


type Msg
    = Move Direction
    | GotImage (Result Http.Error (Maybe Image))
      -- | ChangeInteractionMode
      -- | ChangeUiFocus
    | SecondaryButton1
    | SecondaryButton2
    | TileClick ( Position, WorldTile )


init : ( GameState, Cmd Msg )
init =
    ( { mainPlayer =
            { facing = Right
            , position = ( 5, 5 )
            }
      , otherPlayers = Dict.empty
      , map = Dict.empty
      , cameraCenterPos = ( 0, 0 )
      , actionBar =
            { items = Array.repeat 5 Nothing |> Array.set 0 (Just WorldTile.Hoe)
            , selected = 0
            }
      }
    , getImage "town1.png"
    )


updateGame : Msg -> GameState -> ( GameState, Cmd Msg )
updateGame msg gameState =
    case msg of
        Move direction ->
            let
                newMainPlayer =
                    Player.move gameState.map direction gameState.mainPlayer

                newCameraCenterPos =
                    newMainPlayer.position
            in
            ( { gameState | mainPlayer = newMainPlayer, cameraCenterPos = newCameraCenterPos }, Cmd.none )

        GotImage imageResult ->
            case imageResult of
                Ok imageMaybe ->
                    case imageMaybe of
                        Just image ->
                            let
                                ( map, playerPos ) =
                                    imageToMap image
                            in
                            ( { gameState | map = map, mainPlayer = Player.updatePlayer playerPos gameState.mainPlayer, cameraCenterPos = playerPos }, Cmd.none )

                        Nothing ->
                            ( gameState, Debug.log "Image file corrupted" Cmd.none )

                Err httpError ->
                    let
                        error =
                            case httpError of
                                Http.Timeout ->
                                    "Timeout exceeded"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus status ->
                                    String.fromInt status

                                Http.BadBody resp ->
                                    "Unexpected response from api: " ++ resp

                                Http.BadUrl url ->
                                    "Malformed url: " ++ url
                    in
                    ( gameState, Debug.log error Cmd.none )

        -- ChangeInteractionMode ->
        --     let
        --         newMode =
        --             case gameState.interactionMode of
        --                 Playing ->
        --                     Editing CharacterFocus
        --                 Editing ->
        --                     Playing
        --     in
        --     ( {gameState | interactionMode = newMode }, Cmd.none )
        -- ChangeUiFocus ->
        --     case gameState.interactionMode of
        --         Playing ->
        --             ( gameState, Cmd.none )
        --         Editing uiFocus ->
        --             (
        --                 { gameState
        --                 | interactionMode
        --                     = case uiFocus of
        --                         CharacterFocus ->
        --                             ActionBarFocus
        --                         ActionBarFocus ->
        --                             CharacterFocus
        --                 }
        --             , Cmd.none)
        SecondaryButton1 ->
            ( { gameState | actionBar = updateActionBar (gameState.actionBar.selected - 1) gameState.actionBar }, Cmd.none )

        SecondaryButton2 ->
            ( { gameState | actionBar = updateActionBar (gameState.actionBar.selected + 1) gameState.actionBar }, Cmd.none )

        TileClick ( tilePosition, tile ) ->
            let
                inRange =
                    gameState.mainPlayer.position
                        |> manhattanDistance tilePosition
                        |> (>) 2

                newWorldTile =
                    if inRange then
                        Array.get gameState.actionBar.selected gameState.actionBar.items
                            |> Maybe.withDefault Nothing
                            |> Maybe.map (interactOnTile tile)
                            |> Maybe.withDefault tile

                    else
                        tile
            in
            ( { gameState | map = Dict.insert tilePosition newWorldTile gameState.map }, Cmd.none )


interactOnTile : WorldTile -> WorldTile -> WorldTile
interactOnTile worldTile item =
    case ( worldTile, item ) of
        ( WorldTile.Dirt, WorldTile.Hoe ) ->
            WorldTile.HoedDirt

        _ ->
            worldTile


updateActionBar : Int -> ActionBar -> ActionBar
updateActionBar newSelected actionBar =
    let
        actionBarLength =
            Array.length actionBar.items

        selected =
            newSelected |> modBy actionBarLength
    in
    { actionBar | selected = selected }


getImage : String -> Cmd Msg
getImage path =
    Http.get
        { url = path
        , expect = Http.expectBytesResponse GotImage httpReponseToImage
        }


httpReponseToImage : Http.Response Bytes -> Result Http.Error (Maybe Image)
httpReponseToImage response =
    case response of
        Http.GoodStatus_ _ body ->
            Ok <| Image.decode body

        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)


imageToMap : Image -> ( Dict Position WorldTile, Position )
imageToMap image =
    let
        list2d =
            Image.toList2d image

        pixels =
            List.indexedMap (\r row -> List.indexedMap (\c x -> ( ( c, r ), x )) row) list2d
                |> List.concat
                |> Dict.fromList
                |> Dict.map
                    (\_ v ->
                        if v >= 0 then
                            v

                        else
                            maxInt + v
                    )

        -- playerInitialSearch = Array.filter (\ar -> Array.filter (\x -> x == playerColor) ar) pixels
        playerInitialPosM =
            pixels
                |> Dict.filter (\_ v -> v == playerColor)
                |> Dict.keys
                |> List.head
    in
    ( pixels
        |> Dict.map (\_ v -> pixelToTile v)
        |> Dict.toList
        |> List.filterMap (\x -> Tuple.second x |> Maybe.map (Tuple.pair <| Tuple.first x))
        |> Dict.fromList
    , Maybe.withDefault ( 0, 0 ) playerInitialPosM
    )


pixelToTile : Image.Pixel -> Maybe WorldTile
pixelToTile pixel =
    case pixel of
        0xFF0000FF ->
            Just WorldTile.WallTile

        0xFFFFFFFF ->
            Just WorldTile.Dirt

        _ ->
            Nothing


generateActionBar : UiFocus -> Int -> Int -> ActionBar -> List ( Position, Tile Msg )
generateActionBar uiFocus vpHeight vpWidth { items, selected } =
    let
        widthMiddle =
            toFloat vpWidth / 2 |> floor

        numItems =
            Array.length items

        numItemsLHalve =
            toFloat numItems / 2 |> floor

        -- numItemsRHalve = numItems - numItemsLHalve
    in
    items
        |> Array.indexedMap
            (\idx item ->
                let
                    markerTile =
                        case ( uiFocus, idx == selected ) of
                            ( ActionBarFocus, True ) ->
                                Tile.fromPosition ( 4, 2 )

                            ( ActionBarFocus, False ) ->
                                Tile.fromPosition ( 4, 1 )

                            ( CharacterFocus, True ) ->
                                Tile.fromPosition ( 4, 2 )

                            ( CharacterFocus, False ) ->
                                Tile.fromPosition ( 4, 0 )
                in
                ( ( widthMiddle - numItemsLHalve + idx, vpHeight - 1 )
                , Tile.multipleTiles
                    (Tile.fromPosition ( 4, 3 )
                        :: Maybe.withDefault [] (Maybe.map (toGameTiles ( 0, 0 ) >> List.singleton) item)
                        ++ [ markerTile ]
                    )
                )
            )
        |> Array.toList


getGameTiles : GameState -> List ( Position, Tile Msg )
getGameTiles gameState =
    let
        overlayTiles =
            -- case gameState.interactionMode of
            --     Playing ->
            --         []
            --     Editing ->
            generateActionBar ActionBarFocus rows rows gameState.actionBar

        -- [ (((toFloat rows)/2 |> floor, rows-1), Tile.fromPosition (4, 0))
        -- ]
    in
    (((gameState.map
        |> visibleTiles gameState.cameraCenterPos
        |> Dict.map (\k v -> toGameTiles k v)
        |> Dict.toList
       -- |> List.map (\((x, y), t) -> ((x, y), Tile.movable ("tile" ++ String.fromInt (x*1000 + y)) t))
      )
        ++ [ ( gameState.mainPlayer.position, playerTile gameState.mainPlayer |> Tile.movable "mainPlayer" ) ]
     )
        |> List.map (frameWithCameraCenterPos gameState.cameraCenterPos)
    )
        |> (\x -> x ++ overlayTiles)


areas : GameState -> List (Area Msg)
areas gameState =
    [ PixelEngine.tiledArea
        { rows = rows
        , tileset =
            { source = "tileset.png"
            , spriteWidth = 16
            , spriteHeight = 16
            }
        , background =
            PixelEngine.colorBackground <| Color.rgb255 0 0 0
        }
        (getGameTiles gameState)
    ]


toGameTiles : Position -> WorldTile -> Tile Msg
toGameTiles ( x, y ) worldTile =
    (case worldTile of
        WorldTile.WallTile ->
            Tile.multipleTiles
                [ toGameTiles ( x, y ) WorldTile.Dirt
                , Tile.fromPosition ( 2, 0 )
                ]

        WorldTile.Dirt ->
            Tile.fromPosition ( 1, Tuple.first (Random.step (Random.int 0 2) (Random.initialSeed ((x * 1000) + y))) )

        WorldTile.Hoe ->
            Tile.fromPosition ( 5, 0 )

        WorldTile.HoedDirt ->
            Tile.fromPosition ( 1, 3 )
    )
        |> Tile.clickable (TileClick ( ( x, y ), worldTile ))


visibleTiles : Position -> Dict Position WorldTile -> Dict Position WorldTile
visibleTiles center tiles =
    tiles
        |> Dict.filter
            (\k _ ->
                Position.coordsTo center k
                    |> (\{ x, y } -> toFloat (abs x) < toFloat rows / 2 && toFloat (abs y) < toFloat rows / 2)
            )


playerTile : Player -> Tile Msg
playerTile player =
    case player.facing of
        Down ->
            Tile.fromPosition ( 3, 0 )

        Up ->
            Tile.fromPosition ( 3, 2 )

        Left ->
            Tile.fromPosition ( 3, 1 ) |> Tile.withAttributes [ style "transform" "scaleX(-1)" ]

        Right ->
            Tile.fromPosition ( 3, 1 )


viewportCenter : Position
viewportCenter =
    let
        center =
            floor (toFloat rows / 2)
    in
    ( center, center )


frameWithCameraCenterPos : Position -> ( Position, Tile Msg ) -> ( Position, Tile Msg )
frameWithCameraCenterPos cameraCenterPosition ( position, tile ) =
    let
        vpCenterCoords =
            Position.coordsTo viewportCenter ( 0, 0 )

        cameraCenterCoords =
            Position.coordsTo cameraCenterPosition ( 0, 0 )
    in
    Position.add vpCenterCoords position
        |> Position.add (Position.scaleBy -1 cameraCenterCoords)
        |> (\p -> Tuple.pair p tile)
