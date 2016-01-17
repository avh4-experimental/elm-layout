module Layout.Core (Layout, mapBounds, custom, toHtml) where

{-| This module defines the basic types that layouts are built from.

-}

import Html exposing (Html)


type Layout bounds
  = VirtualDom (bounds -> Html)


mapBounds : (a -> b) -> Layout b -> Layout a
mapBounds fn l =
  case l of
    VirtualDom render ->
      VirtualDom (fn >> render)


custom : (bounds -> Html) -> Layout bounds
custom render =
  VirtualDom render


toHtml : bounds -> Layout bounds -> Html
toHtml bounds layout =
  case layout of
    VirtualDom render ->
      render bounds
