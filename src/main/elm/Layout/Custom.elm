module Layout.Custom (RectangularBounds, html) where

{-| This module allows you to define your own custom layout types.

-}

import Html exposing (Html)
import Layout.Core as Core exposing (Layout)


type alias RectangularBounds =
    { x : Float, y : Float, w : Float, h : Float }


html : (RectangularBounds -> Html) -> Layout RectangularBounds
html render =
    Core.custom render
