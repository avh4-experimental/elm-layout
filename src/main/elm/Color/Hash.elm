module Color.Hash (fromString) where

import Color exposing (Color)
import String
import Char

prime1 = 31
prime2 = 37
prime3 = 41
largePrime = 569

fromString : String -> Color
fromString string =
    let
        inc k i x = ((x*k + i)*k) % largePrime
        step (h,s,l) string =
            case String.uncons string of
                Nothing -> (h,s,l)
                Just (ch,rest) ->
                    let i = Char.toCode ch
                    in step (inc prime1 i h, inc prime2 i s, inc prime3 i l) rest
        (h,s,l) = step (0,0,0) string
    in
        Color.hsl
            ((toFloat h)*360/largePrime |> degrees)
            ((toFloat s)/largePrime*0.7 + 0.3)
            ((toFloat l)/largePrime*0.60 + 0.35)
