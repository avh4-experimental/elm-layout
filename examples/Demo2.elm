module Main (..) where

import Layout
import Window
import Color
import Layout


thing =
  Layout.placeholder { myData = {} }


squareOnBlue thing =
  Layout.stack
    [ Layout.fill Color.darkBlue
    , thing
      |> Layout.square
      |> Layout.inset 10

    ]


view =
  thing
    |> squareOnBlue


main =
  Layout.toFullWindow (Signal.constant (Layout.inset 50 view))
