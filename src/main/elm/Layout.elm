module Layout
    ( Layout
    , text
    , placeholder, image, croppedImage
    , fill
    , inset, top, flow, stack, list
    , onClick
    , toHtml, toFullWindow) where

{-| An experimental alternative to Graphics.Element and elm-html

The concept being explored by this rendering library is to have the core element
type be a function of `{x,y,w,h} -> rendering` instead of simply being a rendering.
This leads to some interesting properties with respect to creating dynamic layouts,
and thus far appears to give a nice API for quickly creating and modifying layouts.
It also provides a mechanism for creating reusable layout logic.

@docs Layout

## Basic elements

@docs placeholder, text, image, croppedImage, fill

## Positioning

@docs inset, top

## Lists

@docs flow, stack, list

## Events

@docs onClick

## Integration

@docs toHtml, toFullWindow

-}

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Json
import Color exposing (Color)
import Color.Hash
import Window

type alias Bounds = {x:Float, y:Float, w:Float, h:Float}

type alias Image = String

{-| A graphical element that will be rendered into a particular bounds at a later time.
-}
type Layout = Layout (Bounds -> Html)

colorToString c =
    let {red,green,blue,alpha} = Color.toRgb c
    in "rgb(" ++ (toString red) ++ ", " ++ (toString green) ++ ", " ++ (toString blue) ++ ")"


div : Bounds -> List (String,String) -> List Html.Attribute -> List Html -> Html
div {x,y,w,h} styles attrs children =
    Html.div ((Html.style
        ([ ("position", "absolute")
                , ("width", (toString w) ++ "px")
                , ("height", (toString h) ++ "px")
                , ("top", (toString y) ++ "px")
                , ("left", (toString x) ++ "px")
                ] ++ styles)) :: attrs) children


{-| An element intended to be a placeholder for something that will be implemented at
a later time.  The placeholder will show the given text and will use the text to generate a
unique color to fill the placeholder's bounds.

This element is intended to aid in quick prototyping where you want to reserve an area on the screen
to implement at a later time.

    view = Layout.top 50
        (Layout.placeholder "header")
        (Layout.placeholder "content")
-}
placeholder : String -> Layout
placeholder s =
    stack
        [ fill (Color.Hash.fromString s)
        , text {size=12, color=Color.darkCharcoal} s
        ]


{-| An element that renders text with a given style.

    Layout.text {size=32,color=Color.darkCharcoal} "Welcome"
-}
text : {size:Int,color:Color} -> String -> Layout
text style s =
    Layout <| \bounds ->
        div bounds
            [ ("overflow", "auto")
            , ("font-size", (toString style.size) ++ "px")
            , ("color", colorToString style.color)
            ] []
            [ Html.text s ]

{-| An element that renders an image.

    Layout.image "mario.png"
-}
image : Image -> Layout
image src =
    Layout <| \bounds ->
        let
            background = "url(" ++ src ++ ")"
        in
            div bounds
                [ ("background-image", background)
                ]
                [] []

{-| An image cropped to a specific region of the source image.
The cropped region will be scaled to fill the bounds that this element renders into.

    Layout.croppedImage 50 70 "tiles50x70.png" 10 10 20 30
-}
croppedImage : Float -> Float -> Image -> Float -> Float -> Float -> Float -> Layout
croppedImage sw sh src iw ih ix iy =
    Layout <| \bounds ->
        let
            {w,h} = bounds
            offsetX = (ix * w / iw |> toString) ++ "px"
            offsetY = (iy * h / ih |> toString) ++ "px"
            background = "url(" ++ src ++ ") " ++ offsetX ++ " " ++ offsetY
            size = (toString (sw * w / iw)) ++ "px " ++ (toString (sh * h / ih)) ++ "px"
        in
            div bounds
                [ ("background", background)
                , ("background-size", size)
                ]
                [] []

--
-- Shapes
--

{-| An element that fills its bounds with a color.

    Layout.fill Color.red
-}
fill : Color -> Layout
fill c =
    Layout <| \bounds ->
        div bounds
            [ ("background-color", colorToString c)
            ]
            [] []

--
-- Layout
--

--position : Float -> Float -> Float -> Float -> Layout -> Layout
--position x y w h g =
--    Layout <| \bounds ->
--        toHtml' {x=x,y=y,w=w,h=h} g

{-| A container element that inserts padding around its child

    withBorder child = Layout.stack
        [ Layout.fill Color.grey
        , Layout.inset 2 child
        ]
-}
inset : Float -> Layout -> Layout
inset i g =
    Layout <| \bounds ->
        toHtml' {x=bounds.x+i,y=bounds.y+i,w=bounds.w-i-i,h=bounds.h-i-i} g

{-| Position two elements vertically, with the first element taking a given height

    Layout.top 50
        (Layout.placeholder "header")
        (Layout.placeholder "content")
-}
top : Float -> Layout -> Layout -> Layout
top ih a b =
    Layout <| \bounds ->
        div bounds [] []
            [ toHtml' {x=0,w=bounds.w,y=0,h=ih} a
            , toHtml' {x=0,w=bounds.w,y=ih,h=bounds.h-ih} b
            ]

{-| An element that renders a list of children into bounds of a given size and
lays them out in a left-to-right flow that wraps at this element's bounds

    Layout.flow (32,32) (List.map fill [Color.red, Color.blue])
-}
flow : (Float,Float) -> List Layout -> Layout
flow (iw,ih) items =
    Layout <| \bounds ->
        div bounds [ ("overflow", "auto") ] []
            ( List.map (\i -> Html.div [Html.style
                [ ("position", "relative")
                , ("width", (toString iw) ++ "px")
                , ("height", (toString ih) ++ "px")
                , ("display", "inline-block")
                , ("vertical-align", "bottom")
                ]] [toHtml' {x=0,y=0,w=iw,h=ih} i]) items )

{-| An element that renders a list of children on top of one another in the same bounds.

    Layout.stack
        [ Layout.image "background.png"
        , Layout.text "Welcome"
        ]
-}
stack : List Layout -> Layout
stack items =
    Layout <| \bounds ->
        items
        |> List.map (toHtml' {bounds | x = 0, y = 0})
        |> div bounds [] []

{-| An element that renders a list of children in a vertical list with a given height.

The list will scroll vertically if there are enough children to exceed the vertical bounds of the list.

    Layout.list 44 (List.map placeholder ["Item 1", "Item 2", "Item 3"])
-}
list : Float -> List Layout -> Layout
list ih items =
    Layout <| \bounds ->
        toHtml' bounds (flow (bounds.w,ih) items)

--
-- Events
--

{-| Adds a click listener to an element
-}
onClick : Signal.Message -> Layout -> Layout
onClick message item =
    Layout <| \bounds ->
        div bounds []
            [ Html.on "click" Json.value (\_ -> message) ]
            [ toHtml' {bounds | x = 0, y = 0} item ]

--
-- Integration
--

toHtml' : Bounds -> Layout -> Html
toHtml' bounds g =
    case g of
        Layout fn -> fn bounds

{-| Render a Layout to Html.

    view = Layout.placeholder "view"
    main = Layout.toHtml (800, 600) view
-}
toHtml : (Int,Int) -> Layout -> Html
toHtml (w,h) =
    toHtml' {x=0,y=0,w=toFloat w,h=toFloat h}


{-| Simplifies rendering an element to fill the window.

    view = Layout.placeholder "view"
    main = Layout.toFullWindow (Signal.constant view)
-}
toFullWindow : Signal Layout -> Signal Html
toFullWindow viewSignal = Signal.map2 toHtml Window.dimensions viewSignal
