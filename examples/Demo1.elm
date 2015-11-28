module Main (..) where

import Layout
import Window


view =
    Layout.top
        48
        (Layout.placeholder "header")
        ([1..25]
            |> List.map (\n -> "Item " ++ (toString n))
            |> List.map Layout.placeholder
            |> Layout.list 32
            |> Layout.inset 8
        )


main =
    Layout.toFullWindow (Signal.constant view)
