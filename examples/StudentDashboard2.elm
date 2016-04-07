module StudentDashboard (..) where

import Layout
import Window
import Color
import Layout

-- Header is static.  Content and mosaic scroll.  mosaic grows to height of content

view =
  Layout.placeholder "Student Dashboard"
    |> withMosaic
    |> Layout.withHeight 500



-- Layout.sequence
--   [ pageHeader
--       |> Layout.withHeight 73
--   , Layout.placeholder "Student Dashboard"
--       |> Layout.withHeight 500
--
--   ]
-- Layout.placeholder "Student Dashboard"
--   |> Layout.fixedWidth 500
--   |> withMosaic
--   |> Layout.top 73 pageHeader


pageHeader =
  Layout.placeholder "Page Header"
    |> Layout.bottom 3 (Layout.fill Color.darkPurple)


withMosaic inner =
  inner
    |> Layout.left 130 mosaic
    |> Layout.right 130 mosaic


mosaic =
  Layout.placeholder "Interests mosaic"


main =
  Layout.toPage (Signal.constant view)
