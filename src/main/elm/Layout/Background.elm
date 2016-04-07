module Layout.Background (Background, blank, placeholder, fill, left, right, toHtml) where

{-| A Background is a function from a width to Html, where the resulting Html
will have height:100%

## Basic backgrounds

@docs blank, placeholder, fill

## Layout

@docs left, right

## Integration

@docs toHtml
-}

import Layout.Color
import Html exposing (Html)
import Html.Attributes as Html
import Color exposing (Color)


type Background
  = Background ({ l : Float, w : Float } -> Html)


div styles attrs children { w, l } =
  let
    styles' =
      [ ( "width", toString w ++ "px" )
      , ( "left", toString l ++ "px" )
      , ( "height", "100%" )
      , ( "position", "absolute" )
      ]
        ++ styles

    attrs' =
      [ Html.style styles'
      ]
        ++ attrs
  in
    Html.div attrs' children


{-| -}
blank : Background
blank =
  fill (Color.rgba 0 0 0 0.0)


{-| -}
placeholder : anything -> Background
placeholder s =
  Background
    <| div
        [ ( "background-color", "#eee" )
        , ( "border", "1px solid #ccc" )
        , ( "box-sizing", "border-box" )
        , ( "color", "#aaa" )
        , ( "text-align", "center" )
        , ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "flex-direction", "column" )
        ]
        []
        [ Html.text <| toString s ]


{-| -}
fill : Color -> Background
fill color =
  Background
    <| div
        [ ( "background-color", Layout.Color.toString color )
        ]
        []
        []



---
--- Layout
---


left : Float -> Background -> Background -> Background
left w l r =
  Background
    <| \bounds ->
        div
          []
          []
          [ toHtml' { l = 0, w = w } l
          , toHtml' { l = w, w = bounds.w - w } r
          ]
          bounds


right : Float -> Background -> Background -> Background
right w r l =
  Background
    <| \bounds ->
        div
          []
          []
          [ toHtml' { l = 0, w = bounds.w - w } l
          , toHtml' { w = w, l = bounds.w - w } r
          ]
          bounds



---
--- Integration
---


{-| -}
toHtml' : { w : Float, l : Float } -> Background -> Html
toHtml' bounds (Background background) =
  background bounds


{-| -}
toHtml : Float -> Background -> Html
toHtml width (Background background) =
  background { w = width, l = 0 }
