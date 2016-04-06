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
  , ( "Positioning"
    , [ ( "inset"
        , Layout.fill Color.darkGray
            |> Layout.inset 20
        )
      , ( "top"
        , Layout.fill Color.darkGray
            |> Layout.top 25 (Layout.fill Color.darkOrange)
        )
      , ( "bottom"
        , Layout.fill Color.darkGray
            |> Layout.bottom 25 (Layout.fill Color.darkOrange)
        )
      , ( "left"
        , Layout.fill Color.darkGray
            |> Layout.left 25 (Layout.fill Color.darkOrange)
        )
      , ( "right"
        , Layout.fill Color.darkGray
            |> Layout.right 25 (Layout.fill Color.darkOrange)
        )
      , ( "center"
        , Layout.fill Color.darkGray
            |> Layout.center (always { w = 200, h = 50 })
        )
      , ( "square"
        , Layout.fill Color.darkGray
            |> Layout.square
        )
      ]
    )
  , ( "Lists"
    , [ ( "flow"
        , [ Color.darkRed, Color.darkOrange, Color.darkYellow, Color.darkGreen, Color.darkBlue, Color.darkPurple ]
            |> List.map Layout.fill
            |> Layout.flow ( 120, 60 )
        )
      , ( "stack"
        , Layout.stack
            [ Layout.text { size = 60, color = Color.rgba 30 0 0 0.5 } "lazy"
            , Layout.text { size = 60, color = Color.rgba 0 30 0 0.5 } "brown"
            , Layout.text { size = 60, color = Color.rgba 0 0 30 0.5 } "fox"
            ]
        )
      , ( "list"
        , [1..50]
            |> List.map (toString >> Layout.text { size = 8, color = Color.darkCharcoal })
            |> Layout.list 10
        )
      ]
    )
  ]
    |> List.map section
    |> List.concat
    |> Layout.list 100


section : ( String, List ( String, Layout ) ) -> List Layout
section ( name, examples ) =
  (Layout.placeholder name) :: (List.map example examples)


example : ( String, Layout ) -> Layout
example ( name, layout ) =
  layout
    |> Layout.left 150 (Layout.placeholder name)


main =
  Layout.toFullWindow (Signal.constant view)
