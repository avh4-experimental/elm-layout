module StudentDashboard (..) where

import Layout
import Window
import Color
import Layout
import Layout.Background as Background exposing (Background)


-- Header, content and mosaic scroll.  mosaic grows to height of content


text =
  { title = Layout.text { size = 26, color = Color.darkCharcoal }
  }


view =
  Layout.sequence
    [ pageHeader
        |> Layout.withHeight 73
    , Layout.sequence
        [ title
        , performance
        , assignments
        ]
        |> Layout.fixedWidth 500
        |> Layout.withBackground (Background.blank |> withMosaic)
    , pageFooter
        |> Layout.withHeight 73
    ]


pageHeader =
  Layout.placeholder "Page Header"
    |> Layout.bottom 3 (Layout.fill Color.darkPurple)


pageFooter =
  Layout.placeholder "Page Footer"
    |> Layout.top 3 (Layout.fill Color.darkCharcoal)


withMosaic inner =
  inner
    |> Background.left 130 mosaic
    |> Background.right 130 mosaic


mosaic =
  Background.placeholder "Interests mosaic"


title =
  text.title "Hi Christopher!"
    |> Layout.withHeight 102


performance =
  Layout.placeholder "performance"
    |> Layout.withHeight 102


assignments =
  Layout.sequence
    [ text.title "Your Assignments" |> Layout.withHeight 102
    , Layout.placeholder "assignments" |> Layout.withHeight 102
    ]


main =
  Layout.toPage (Signal.constant view)
