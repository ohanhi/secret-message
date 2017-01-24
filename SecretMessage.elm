module SecretMessage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Mouse exposing (Position)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


fontSize : Int
fontSize =
    20



-- MODEL


type alias Model =
    { position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Position (fontSize * 15) 0) Nothing, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag } as model) =
    case msg of
        DragStart xy ->
            Model position (Just (Drag xy xy))

        DragAt xy ->
            Model position (Maybe.map (\{ start } -> Drag start xy) drag)

        DragEnd _ ->
            Model (getPosition model) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


view : Model -> Html Msg
view model =
    div
        [ style
            [ "position" => "relative"
            , "overflow" => "hidden"
            , "-webkit-user-select" => "none"
            , "-moz-user-select" => "none"
            , "-ms-user-select" => "none"
            , "user-select" => "none"
            , "height" => px (fontSize * 13)
            ]
        ]
        [ pre
            [ style (commonStyles ++ (specialCodecardStyles (getPosition model)))
            ]
            [ text """ What's awesome?

HGLNAMVAPAKEADFN
LZVCY2PEOUFDS0AW
[DA02M;VXYUNBTRS
1SADFRIO2EVUNAA]
BAM,012L;ZKFL;AS
ZAFJJMMA;LCVMQPV
P<L,MZCXJI20<3<;
-F1L;VFJ29DKZXMQ

"""
            ]
        , pre
            [ style (commonStyles ++ (specialDraggableStyles (getPosition model)))
            , onMouseDown
            ]
            [ text """

████████████████
████ ███  ██████
████████████████
██ ██ ███ ██████
████████████████
████████████████
████████████  ██
████████████████
      (Drag me!)
"""
            ]
        ]


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Model -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)


commonStyles : List ( String, String )
commonStyles =
    [ "padding" => px (fontSize // 2)
    , "border" => "1px solid rgba(0,0,0, 0.5)"
    , "border-radius" => "3px"
    , "line-height" => "1"
    , "font-size" => px fontSize
    ]


snapPosition : a -> (a -> Int) -> Int
snapPosition realPosition pos =
    if (pos realPosition) < 5 then
        0
    else
        pos realPosition


switchingColor : Position -> String
switchingColor position =
    if (snapPosition position .x + snapPosition position .y) == 0 then
        "green"
    else
        "inherit"


specialCodecardStyles : Position -> List ( String, String )
specialCodecardStyles realPosition =
    [ "position" => "absolute"
    , "left" => px 0
    , "top" => px 0
    , "color" => switchingColor realPosition
    ]


specialDraggableStyles : Position -> List ( String, String )
specialDraggableStyles realPosition =
    let
        snap =
            snapPosition realPosition
    in
        [ "position" => "absolute"
        , "left" => px 0
        , "top" => px 0
        , "transform" => ("translate(" ++ px (snap .x) ++ ", " ++ px (snap .y) ++ ")")
        , "color" => "#333"
        , "cursor" => "move"
        , "background" => "transparent"
        , "border-color" => switchingColor realPosition
        ]
