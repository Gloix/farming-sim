module Player exposing (..)

import Dict as Dict exposing (Dict)

import Grid.Position as Position exposing (Position)
import Grid.Direction exposing (Direction(..))

import WorldTile as WorldTile exposing (WorldTile)


type alias Player =
    { facing : Direction
    , position : Position.Position
    }


steppableTiles : List WorldTile
steppableTiles = [WorldTile.Dirt, WorldTile.Hoe, WorldTile.HoedDirt]


move : Dict Position WorldTile -> Direction -> Player -> Player
move map direction player =
    let
        intendedPosition = Position.move 1 direction player.position
        mapTile = Dict.get intendedPosition map
        shouldMove
            =  mapTile
            -- |> Maybe.map (\mt -> (direction == player.facing) && List.member mt steppableTiles)
            |> Maybe.map (\mt -> List.member mt steppableTiles)
            |> Maybe.withDefault False
        newPosition = if shouldMove then intendedPosition else player.position
    in
    { player | facing = direction, position = newPosition }


updatePlayer : Position -> Player -> Player
updatePlayer position player =
    { player | position = position }


type alias PlayerId = String