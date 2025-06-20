module Main exposing (Model, Tile(..), main)

import Html exposing (Html, div, text, p, button, a, span, h1)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Random.List exposing (shuffle)
import Random
import Browser exposing (element)

type Tile
    = Tile Int
    | Collapsed
    | BlueStart
    | RedStart


type Turn
    = Red
    | Blue


-- The tile list is an array, but
-- is a 4x4 grid with x,y coords
-- [
-- 0,0 0,1 0,2 0,3
-- 1,0 1,1 1,2 1,3
-- 2,0 2,1 2,2 2,3
-- 3,0 3,1 3,2 3,3
-- ]

type alias Model =
    { tiles : List Tile
    , turn : Turn
    , movesLeft : Int
    , redAt : ( Int, Int )
    , blueAt : ( Int, Int )
    , collapseNext : ( Int, Int )
    , badMoveReason : Maybe String
    , history : History
    }

type History = History (List Model)


type alias Coord = (Int, Int)


type Msg
    = MoveTo Int Int
    | Reset
    | Undo
    | InvalidMoveAttempted String
    | ShuffledBoard (List Tile)

xyToIndex : Coord -> Int
xyToIndex (x, y) = 4 * x + y

indexToXy : Int -> Coord
indexToXy index = ( index // 4, modBy 4 index )


collapseAt : Coord -> Int -> Tile -> Tile
collapseAt coord currentIndex currentTile =
    if (xyToIndex coord == currentIndex) then
        Collapsed

    else
        currentTile


getTileAt : Coord -> List Tile -> Maybe Tile
getTileAt coord tiles = 
    let aimIndex = xyToIndex coord in
    List.head (List.drop aimIndex tiles)


movesForPlayer : Coord -> Model -> Int
movesForPlayer coord state = case getTileAt coord state.tiles of
    Just tile ->
        case tile of
            Tile tileVal -> tileVal
            _ -> 4 -- This shouldn't happen, except at the start or end of the game
    _ -> 4 -- This should never happen

safeUndoHead : List Model -> (Model, Cmd Msg)
safeUndoHead lst = case List.head lst of
    Just x -> (x, Cmd.none)
    _ -> init ()

addIndexIfMatch : a -> Int -> a -> Int 
addIndexIfMatch target index value = if target == value then index else 0


getRedStart : List Tile -> Coord
getRedStart board = indexToXy (List.sum (List.indexedMap (addIndexIfMatch RedStart) board))

getBlueStart : List Tile -> Coord
getBlueStart board = indexToXy (List.sum (List.indexedMap (addIndexIfMatch BlueStart) board))


update : Msg -> Model -> (Model, Cmd Msg)
update msg state =
    case msg of
        MoveTo x y ->
            let updatedState = { state | badMoveReason = Nothing, tiles = List.indexedMap (collapseAt state.collapseNext) state.tiles } in
            let postMoveState = if state.movesLeft == 1 then if state.turn == Red then
                    { updatedState | redAt = ( x, y ), movesLeft = movesForPlayer state.blueAt state, turn = Blue, collapseNext = state.blueAt} else { updatedState | blueAt = ( x, y ), movesLeft = movesForPlayer state.redAt state, turn = Red, collapseNext = state.redAt } else
                    if state.turn == Red then
                        { updatedState | redAt = ( x, y ), movesLeft = state.movesLeft - 1 }

                    else
                        { updatedState | blueAt = ( x, y ), movesLeft = state.movesLeft - 1 }
            in 
            case state.history of
            History priorHistory -> ({ postMoveState | history = History (List.append [state] priorHistory) }, Cmd.none)
        Reset -> init ()
        Undo -> case state.history of
            History previous -> safeUndoHead previous
        InvalidMoveAttempted badBecause -> ({ state | badMoveReason = Just badBecause }, Cmd.none)
        ShuffledBoard newTiles -> ({ initModel | tiles = newTiles, blueAt = getBlueStart newTiles, redAt = getRedStart newTiles, collapseNext = getBlueStart newTiles }, Cmd.none)


getFromPos : Model -> Coord
getFromPos state = case state.turn of
    Blue -> state.blueAt
    Red -> state.redAt


getOpponentPos : Model -> Coord
getOpponentPos state = case state.turn of
    Blue -> state.redAt
    Red -> state.blueAt


checkTileIsAccesible : Coord -> Model -> Msg
checkTileIsAccesible (toX, toY) state = let (fromX, fromY) = getFromPos state in
    let (oppX, oppY) = getOpponentPos state in
    if (oppX == toX) && (oppY == toY) && (state.movesLeft == 1) then InvalidMoveAttempted "Can't finish on opponent's square" else
        if (abs (fromX - toX)) + (abs (fromY - toY)) == 1 then MoveTo toX toY else
            if (fromX == 3) && (toX == 0) && (fromY == toY) then MoveTo toX toY else
                if (fromY == 3) && (toY == 0) && (fromX == toX) then MoveTo toX toY else
                    if (fromX == 0) && (toX == 3) && (fromY == toY) then MoveTo toX toY else
                        if (fromY == 0) && (toY == 3) && (fromX == toX) then MoveTo toX toY else InvalidMoveAttempted "Can't move to non-adjacent tile."


checkMoveValid : Coord -> Model -> Msg
checkMoveValid coord state = case getTileAt coord state.tiles of 
    Just tile -> case tile of
        Collapsed -> InvalidMoveAttempted "Can't move to collapsed tile"
        _ -> checkTileIsAccesible coord state
    Nothing -> InvalidMoveAttempted "Somehow clicked a tile out of range"


moveMessageFromIndex : Int -> Model -> Msg
moveMessageFromIndex index state = let (x, y) = indexToXy index in
    checkMoveValid (x, y) state


tileAttrs : Int -> Int -> Int -> Model -> List (Html.Attribute Msg)
tileAttrs index blueIndex redIndex state = [
    if index == blueIndex && index == redIndex then style "background" "linear-gradient(to right, blue 0%, blue 50%, red 50%, red 100%)" else
        if index == blueIndex then 
            style "background-color" "blue"
        else 
        if index == redIndex then style "background-color" "red" 
        else style "background-color" "#4e4e4e"
    , style "min-height" "80px"
    , style "line-height" "80px"
    , style "text-align" "center"
    , style "padding" "8px"
    , style "border" "solid 1px black"
    , style "color" "white"
    , (onClick (moveMessageFromIndex index state))]


collapsedAttrs : Int -> Model -> List (Html.Attribute Msg)
collapsedAttrs index state = [
    style "background-color" "white"
    , style "min-height" "80px"
    , style "line-height" "80px"
    , style "text-align" "center"
    , style "border" "solid 1px black"
    , style "padding" "8px"
    , (onClick (moveMessageFromIndex index state))]


renderTile : Model -> Int -> Tile -> Html Msg
renderTile state index tile = let blueAtIndex = xyToIndex state.blueAt in let redAtIndex = xyToIndex state.redAt in case tile of
    Collapsed -> div (collapsedAttrs index state) [text "Collapsed"]
    Tile x -> div (tileAttrs index blueAtIndex redAtIndex state) [text (String.fromInt x)]
    RedStart -> div (tileAttrs index blueAtIndex redAtIndex state) [text "Red Start"]
    BlueStart -> div (tileAttrs index blueAtIndex redAtIndex state) [text "Blue Start"]

boardGridClasses : List (Html.Attribute msg)
boardGridClasses = [ 
    style "display" "grid"
    , style "grid-template-columns" "1fr 1fr 1fr 1fr" ]

renderTurn : Model -> List (Html Msg) 
renderTurn state = case state.turn of 
    Red -> [div [] [ 
        text "It is "
        , span [style "color" "red"] [text "RED"]
        , text ("'s turn - " ++ (String.fromInt state.movesLeft) ++ " moves to go") ]]
    Blue -> [div [] [ 
        text "It is "
        , span [style "color" "blue"] [text "BLUE"]
        , text ("'s turn - " ++ (String.fromInt state.movesLeft) ++ " moves to go") ]]

gameExplain : List (Html Msg) 
gameExplain = [ text "Collapsi was invented by 'Riffle Shuffle and Roll' on youtube, you can watch him explain the rules "
    , a [href "https://www.youtube.com/watch?v=6vYEHdjlw3g"] [text "here."]
    , p [] [text "To play Collapsi take turns making moves. Each turn you must move the same number of times as is shown on your starting tile (the first move you must move 4 times). The winner is the last player able to complete their turn. Each time you take a turn the tile you originate on 'collapses' and is no longer available to move on. You can move up, down, left and right. As well as being able to 'wrap-around' like pacman along columns and rows (so long as where you wrap to hasn't collapsed). You can move 'through' a tile that your opponent is on, but cannot finish on their tile. Good luck trapping your opponent!"]]

view : Model -> Html Msg
view state = div [] [
    h1 [] [text "Collapsi"]
    , div boardGridClasses (List.indexedMap (renderTile state) state.tiles)
    , p [] (renderTurn state)
    , case state.badMoveReason of
        Just reason -> p [ style "color" "red" ] [ text reason ]
        _ -> text ""
    , case state.history of
        History [] -> button [] [text "Undo Last Move"]
        _ -> button [onClick Undo] [text "Undo Last Move"]
    , button [onClick Reset] [text "New Game"]
    , p [] gameExplain ]

messageFromNewBoard : List Tile -> Msg
messageFromNewBoard newBoard = ShuffledBoard newBoard


shuffleBoard : List Tile -> Cmd Msg
shuffleBoard board = Random.generate messageFromNewBoard (shuffle board)


initialBoard : List Tile
initialBoard = [
    Tile 4
    , Tile 3
    , Tile 1
    , RedStart
    , Tile 2
    , Tile 1
    , Tile 2
    , Tile 3
    , Tile 2
    , BlueStart
    , Tile 1
    , Tile 3
    , Tile 1
    , Tile 3
    , Tile 2
    , Tile 1]


initModel : Model
initModel = { tiles = initialBoard
    , turn = Blue
    , movesLeft = 4
    , redAt = ( 0, 3 )
    , blueAt = ( 2, 1 )
    , collapseNext = ( 2, 1 )
    , badMoveReason = Nothing
    , history = History []
    }

init : a -> (Model, Cmd Msg)
init _ = (initModel, (shuffleBoard initModel.tiles))


main : Program () Model Msg
main =
    element { init=init, view=view, update=update, subscriptions=\_ -> Sub.none }


-- TODOs: 
-- ~~Render state of where player is~~
-- ~~Implement 'click' command for move logic~~
-- ~~Display who's turn it is~~
-- ~~Implement a 'reset' to initial state~~
-- ~~Implement an 'undo'~~
-- ~~Add credit to the initial YouTube video for inspo~~
-- ~~Only allow moves when they are valid~~
-- ~~Style it nicer~~
-- ~~Make board generation random~~
-- Don't allow re-visiting squares and update game rules
-- Detect win/loss
-- Add an AI to play against that makes random valid moves