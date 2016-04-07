module Layout.Color (toString) where

import Color exposing (Color)


toString : Color -> String
toString c =
  let
    { red, green, blue, alpha } =
      Color.toRgb c
  in
    if alpha == 1.0 then
      "rgb(" ++ (Basics.toString red) ++ ", " ++ (Basics.toString green) ++ ", " ++ (Basics.toString blue) ++ ")"
    else
      "rgba(" ++ (Basics.toString red) ++ ", " ++ (Basics.toString green) ++ ", " ++ (Basics.toString blue) ++ ", " ++ (Basics.toString alpha) ++ ")"
