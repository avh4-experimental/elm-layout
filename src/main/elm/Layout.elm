module Layout (Layout, text, placeholder, image, croppedImage, svg, fill, inset, top, bottom, square, flow, stack, list, onClick, toHtml, toFullWindow, center) where

{-| An experimental alternative to Graphics.Element and elm-html

The concept being explored by this rendering library is to have the core element
type be a function of `{x,y,w,h} -> rendering` instead of simply being a rendering.
This leads to some interesting properties with respect to creating dynamic layouts,
and thus far appears to give a nice API for quickly creating and modifying layouts.
It also provides a mechanism for creating reusable layout logic.

@docs Layout

## Basic elements

@docs placeholder, text, image, croppedImage, svg, fill

## Positioning

@docs inset, top, bottom, center, square

## Lists

@docs flow, stack, list

## Events

@docs onClick

## Integration

@docs toHtml, toFullWindow

-}

import Color exposing (Color)
import Color.Hash
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Json
import Layout.Custom as Custom
import Layout.Core as Core
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Window


type alias Image =
    String


type alias RectangularBounds =
    Custom.RectangularBounds


type alias Size =
    { w : Float
    , h : Float
    }


{-| A graphical element that will be rendered into a particular bounds at a later time.
-}
type alias Layout =
    Core.Layout RectangularBounds


colorToString c =
    let
        { red, green, blue, alpha } = Color.toRgb c
    in
        "rgb(" ++ (toString red) ++ ", " ++ (toString green) ++ ", " ++ (toString blue) ++ ")"


div styles attrs children { x, w, y, h } =
    let
        styles' =
            [ ( "position", "absolute" )
            , ( "width", (toString w) ++ "px" )
            , ( "height", (toString h) ++ "px" )
            , ( "top", (toString y) ++ "px" )
            , ( "left", (toString x) ++ "px" )
            ]
                ++ styles

        attrs' =
            [ Html.style styles'
            ]
                ++ attrs
    in
        Html.div attrs' children


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
    Custom.html
        <| div
            [ ("background-color", "#eee")
            , ("border", "1px solid #ccc")
            , ("box-sizing", "border-box")
            , ("color", "#aaa")
            , ("text-align", "center")
            , ("display", "flex")
            , ("justify-content", "center")
            , ("flex-direction", "column")
            ]
            []
            [ Html.text s ]


{-| An element that renders text with a given style.

    Layout.text {size=32,color=Color.darkCharcoal} "Welcome"
-}
text : { size : Int, color : Color } -> String -> Layout
text style s =
    Custom.html
        <| div
            [ ( "overflow", "auto" )
            , ( "font-size", (toString style.size) ++ "px" )
            , ( "color", colorToString style.color )
            ]
            []
            [ Html.text s ]


{-| An element that renders an image.

    Layout.image "mario.png"
-}
image : Image -> Layout
image src =
    let
        background = "url(" ++ src ++ ")"

        render =
            div
                [ ( "background-image", background ) ]
                []
                []
    in
        Custom.html render


{-| An image cropped to a specific region of the source image.
The cropped region will be scaled to fill the bounds that this element renders into.

    Layout.croppedImage 50 70 "tiles50x70.png" 10 10 20 30
-}
croppedImage : Float -> Float -> Image -> Float -> Float -> Float -> Float -> Layout
croppedImage sw sh src iw ih ix iy =
    Custom.html
        <| \bounds ->
            let
                { w, h } = bounds

                offsetX = (ix * w / iw |> toString) ++ "px"

                offsetY = (iy * h / ih |> toString) ++ "px"

                background = "url(" ++ src ++ ") " ++ offsetX ++ " " ++ offsetY

                size = (toString (sw * w / iw)) ++ "px " ++ (toString (sh * h / ih)) ++ "px"
            in
                div
                    [ ( "background", background )
                    , ( "background-size", size )
                    ]
                    []
                    []
                    bounds


{-| An element displaying an SVG node
-}
svg : RectangularBounds -> Svg -> Layout
svg viewBox child =
    Custom.html
        <| div
            []
            []
            [ Svg.svg
                [ Svg.version "1.1"
                , Svg.viewBox
                    (toString viewBox.x
                        ++ " "
                        ++ toString viewBox.y
                        ++ " "
                        ++ toString viewBox.w
                        ++ " "
                        ++ toString viewBox.h
                    )
                ]
                [ child ]
            ]



--
-- Shapes
--


{-| An element that fills its bounds with a color.

    Layout.fill Color.red
-}
fill : Color -> Layout
fill c =
    Custom.html
        <| div
            [ ( "background-color", colorToString c )
            ]
            []
            []



--
-- Layout
--
--position : Float -> Float -> Float -> Float -> Layout -> Layout
--position x y w h g =
--    Layout <| \bounds ->
--        Core.toHtml {x=x,y=y,w=w,h=h} g


{-| A container element that inserts padding around its child

    withBorder child = Layout.stack
        [ Layout.fill Color.grey
        , Layout.inset 2 child
        ]
-}
inset : Float -> Layout -> Layout
inset i =
    Core.mapBounds
        <| \bounds ->
            { x = bounds.x + i
            , w = bounds.w - i - i
            , y = bounds.y + i
            , h = bounds.h - i - i
            }


{-| Position two elements vertically, with the first element taking a given height

    Layout.top 50
        (Layout.placeholder "header")
        (Layout.placeholder "content")
-}
top : Float -> Layout -> Layout -> Layout
top ih a b =
    Custom.html
        <| \bounds ->
            div
                []
                []
                [ Core.toHtml { x = 0, w = bounds.w, y = 0, h = ih } a
                , Core.toHtml { x = 0, w = bounds.w, y = ih, h = bounds.h - ih } b
                ]
                bounds


{-| Position two elements vertically, with the first element taking a given height

    Layout.bottom 50
        (Layout.placeholder "footer")
        (Layout.placeholder "content")
-}
bottom : Float -> Layout -> Layout -> Layout
bottom ih a b =
    Custom.html
        <| \bounds ->
            div
                []
                []
                [ Core.toHtml { x = 0, w = bounds.w, y = 0, h = bounds.h - ih } b
                , Core.toHtml { x = 0, w = bounds.w, y = bounds.h - ih, h = ih } a
                ]
                bounds


{-| Makes a centered area of a size that is calculated with the given function.
-}
center : (Size -> Size) -> Layout -> Layout
center mapSize =
    Core.mapBounds
        <| \bounds ->
            let
                { w, h } = mapSize { w = bounds.w, h = bounds.h }

                x = (bounds.w - w) / 2

                y = (bounds.h - h) / 2
            in
                Debug.log "center" { x = bounds.x + x, y = bounds.y + y, w = w, h = h }


{-| Makes a a centered area with width equal to height

    Layout.square (Layout.placeholder "square content")
-}
square : Layout -> Layout
square =
    center
        <| \{ w, h } ->
            let
                size = min w h
            in
                { w = size, h = size }


{-| An element that renders a list of children into bounds of a given size and
lays them out in a left-to-right flow that wraps at this element's bounds

    Layout.flow (32,32) (List.map fill [Color.red, Color.blue])
-}
flow : ( Float, Float ) -> List Layout -> Layout
flow ( iw, ih ) items =
    Custom.html
        <| div
            [ ( "overflow", "auto" ) ]
            []
            (List.map
                (\i ->
                    Html.div
                        [ Html.style
                            [ ( "position", "relative" )
                            , ( "width", (toString iw) ++ "px" )
                            , ( "height", (toString ih) ++ "px" )
                            , ( "display", "inline-block" )
                            , ( "vertical-align", "bottom" )
                            ]
                        ]
                        [ Core.toHtml { x = 0, y = 0, w = iw, h = ih } i ]
                )
                items
            )


{-| An element that renders a list of children on top of one another in the same bounds.

    Layout.stack
        [ Layout.image "background.png"
        , Layout.text "Welcome"
        ]
-}
stack : List Layout -> Layout
stack items =
    Custom.html
        <| \bounds ->
            items
                |> List.map (Core.toHtml { bounds | x = 0, y = 0 })
                |> \x -> div [] [] x bounds


{-| An element that renders a list of children in a vertical list with a given height.

The list will scroll vertically if there are enough children to exceed the vertical bounds of the list.

    Layout.list 44 (List.map placeholder ["Item 1", "Item 2", "Item 3"])
-}
list : Float -> List Layout -> Layout
list ih items =
    Custom.html
        <| \bounds ->
            Core.toHtml bounds (flow ( bounds.w, ih ) items)



--
-- Events
--


{-| Adds a click listener to an element
-}
onClick : Signal.Message -> Layout -> Layout
onClick message item =
    Custom.html
        <| \bounds ->
            div
                [ ("cursor", "pointer") ]
                [ Html.on "click" Json.value (\_ -> message) ]
                [ Core.toHtml { bounds | x = 0, y = 0 } item ]
                bounds



--
-- Integration
--


{-| Render a Layout to Html.

    view = Layout.placeholder "view"
    main = Layout.toHtml (800, 600) view
-}
toHtml : ( Int, Int ) -> Layout -> Html
toHtml ( w, h ) =
    Core.toHtml { x = 0, y = 0, w = toFloat w, h = toFloat h }


{-| Simplifies rendering an element to fill the window.

    view = Layout.placeholder "view"
    main = Layout.toFullWindow (Signal.constant view)
-}
toFullWindow : Signal Layout -> Signal Html
toFullWindow viewSignal =
    Signal.map2 toHtml Window.dimensions viewSignal
