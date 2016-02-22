module Layout.Circular (CircularLayout, canvas, fill, rotate, slice) where

{-| Circular layouts

This module is similar to `Layout`, but provides an API for creating layouts that are elliptical instead of rectangular.  The bounds for each element is `{ a0, a1, r0, r1 }`, representing the lower and upper angle and the lower and upper radius for the region.

@docs CircularLayout

## Integration

@docs canvas

## Basic Elements

@docs fill

## Positioning

@docs rotate, slice

-}

import Color exposing (Color)
import Layout.Core as Core
import Svg exposing (Svg)
import Svg.Attributes as SA
import Layout


colorToString : Color -> String
colorToString c =
  let
    { red, green, blue, alpha } =
      Color.toRgb c
  in
    "rgb(" ++ (toString red) ++ ", " ++ (toString green) ++ ", " ++ (toString blue) ++ ")"


type alias Angle =
  Float


type alias CircularBounds =
  { a0 : Angle
  , a1 : Angle
  , r0 : Float
  , r1 : Float
  }


{-| A graphical element that will be rendered into a particular circular bounds at a later time.
-}
type CircularLayout
  = CircularLayout (CircularBounds -> Svg)


{-| Create a `Layout` that renders a CiruclarLayout within an ellipse filling its bounds.
-}
canvas : CircularLayout -> Layout.Layout
canvas (CircularLayout child) =
  Core.custom
    <| \({ x, w, y, h } as bounds) ->
        Core.toHtml
          bounds
          (Layout.svg
            { x = -w / 2, w = w, y = -w / 2, h = w }
            (child { a0 = 0, a1 = 360, r0 = 0, r1 = w / 2 })
          )



--
-- Shapes
--


{-| An element that fills its bounds with a color.

    Layout.fill Color.red
-}
fill : Color -> CircularLayout
fill c =
  CircularLayout
    <| \{ a0, a1, r0, r1 } ->
        let
          convert a r =
            toString (r * cos (a / 180 * pi))
              ++ ","
              ++ toString (r * sin (a / 180 * pi))

          p0i =
            convert a0 r0

          p0o =
            convert a0 r1

          p1i =
            convert a1 r0

          p1o =
            convert a1 r1

          r0' =
            toString r0

          r1' =
            toString r1

          large =
            if a1 - a0 > 180 then
              "1"
            else
              "0"

          path =
            ("M " ++ p0i)
              ++ (" L " ++ p0o)
              ++ (" A " ++ r1' ++ " " ++ r1' ++ " 0 " ++ large ++ " 1 " ++ p1o)
              ++ (" L " ++ p1i)
              ++ (" A " ++ r0' ++ " " ++ r0' ++ " 0 " ++ large ++ " 0 " ++ p0i)
        in
          Svg.path [ SA.d path, SA.fill (colorToString c) ] []



--
-- Layout
--


{-| Shift the angle of a child
-}
rotate : Float -> CircularLayout -> CircularLayout
rotate deg (CircularLayout child) =
  CircularLayout
    <| \{ a0, a1, r0, r1 } ->
        child { a0 = a0 + deg, a1 = a1 + deg, r0 = r0, r1 = r1 }


{-| Divide the region into multiple children by angle.
-}
slice : List ( Float, CircularLayout ) -> CircularLayout
slice children =
  let
    step ( w, child ) ( acc, i ) =
      ( ( i, i + w, child ) :: acc, i + w )

    ( children', total ) =
      List.foldl step ( [], 0 ) children
  in
    CircularLayout
      <| \{ a0, a1, r0, r1 } ->
          let
            a w =
              a0 + (w / total) * (a1 - a0)

            render ( w0, w1, CircularLayout c ) =
              c { a0 = a w0, a1 = a w1, r0 = r0, r1 = r1 }
          in
            children'
              |> List.map render
              |> Svg.g []
