module Reference (..) where

import Layout exposing (Layout)
import Color
import Svg
import Svg.Attributes as Svg


view =
  [ ( "Basic elements"
    , [ ( "placeholder"
        , Layout.placeholder { myValue = Ok 42 }
        )
      , ( "text"
        , Layout.text
            { size = 32, color = Color.darkPurple }
            "My Text"
        )
      , ( "image"
        , Layout.image "https://www.gravatar.com/avatar/0d41db2d5e040f2207789fd24710f93b?s=50&r=pg"
        )
      , ( "tiledImage"
        , Layout.tiledImage "https://www.gravatar.com/avatar/0d41db2d5e040f2207789fd24710f93b?s=50&r=pg"
        )
      , ( "croppedImage"
        , Layout.croppedImage 200 200 "https://www.gravatar.com/avatar/0d41db2d5e040f2207789fd24710f93b?s=200&r=pg" 100 50 0 0
        )
      , ( "svg"
        , Layout.svg
            { x = 0, y = 0, w = 100, h = 100 }
            (Svg.circle [ Svg.cx "50", Svg.cy "50", Svg.r "50" ] [])
        )
      , ( "fill"
        , Layout.fill Color.darkOrange
        )
      ]
    )
  ]
    |> List.map section
    |> List.concat
    |> Layout.list 100

section : (String, List (String, Layout)) -> List Layout
section (name, examples) =
  (Layout.placeholder name) :: (List.map example examples)

example : ( String, Layout ) -> Layout
example ( name, layout ) =
  layout
    |> Layout.left 150 (Layout.placeholder name)


main =
  Layout.toFullWindow (Signal.constant view)
