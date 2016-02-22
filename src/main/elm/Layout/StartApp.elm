module Layout.StartApp (Config, App, start) where

{-| A `evancz/start-app`-like wrapper for `Layout`

@docs Config, App

@docs start

-}

import Layout exposing (Layout)
import StartApp
import Effects exposing (Effects, Never)


{-| Like `StartApp.Config`, except the view produces `Layout` instead of `Html`.
-}
type alias Config model action =
  { init : ( model, Effects action )
  , update : action -> model -> ( model, Effects action )
  , view : Signal.Address action -> model -> Layout
  , inputs : List (Signal action)
  }


{-| An alias for StartApp.App
-}
type alias App model =
  StartApp.App model


{-| Like `StartApp.start`, except taking a Config with a view the produces `Layout` instead of `Html`.
-}
start : Config model action -> App model
start config =
  let
    singleton action =
      [ action ]

    -- messages : Signal.Mailbox (List action)
    messages =
      Signal.mailbox []

    -- address : Signal.Address action
    address =
      Signal.forwardTo messages.address singleton

    -- updateStep : action -> (model, Effects action) -> (model, Effects action)
    updateStep action ( oldModel, accumulatedEffects ) =
      let
        ( newModel, additionalEffects ) =
          config.update action oldModel
      in
        ( newModel, Effects.batch [ accumulatedEffects, additionalEffects ] )

    -- update : List action -> (model, Effects action) -> (model, Effects action)
    update actions ( model, _ ) =
      List.foldl updateStep ( model, Effects.none ) actions

    -- inputs : Signal (List action)
    inputs =
      Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) config.inputs)

    -- effectsAndModel : Signal (model, Effects action)
    effectsAndModel =
      Signal.foldp update config.init inputs

    model =
      Signal.map fst effectsAndModel
  in
    { html =
        model
          |> Signal.map (config.view address)
          |> Layout.toFullWindow
    , model = model
    , tasks =
        effectsAndModel
          |> Signal.map (Effects.toTask messages.address << snd)
    }
