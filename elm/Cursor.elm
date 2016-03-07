module Cursor (Cursor, (>=>), createC, drawC, getC, setC, updateC) where

{-| Alternative way to build elm app,
    this module contains all low level primitives
    to build elm app with high level cursor abstraction.

# Cursor
@docs Cursor

# Composition operator
@docs (>=>)

# Cursor helpers
@docs createC, drawC

# Cursor modifie
@docs getC, setC ,updateC

-}

import Focus exposing ((=>))


-- import Debug exposing (..)


{-| Base type, the triplet of state, signal ans lens
-}
type alias Cursor a b =
  { receiver : Signal.Address a
  , state : a
  , lens : Focus.Focus a b
  , signal : Signal.Signal a
  }


{-| Composition operator, allow to go deeper in state via lens
-}
(>=>) : Cursor a b -> Focus.Focus b c -> Cursor a c
(>=>) c1 f1 =
  { c1 | lens = (c1.lens => f1) }


idL : Focus.Focus a a
idL =
  Focus.create identity (\f r -> f r)


{-| Helper to create a Cursor form signal mail box and initia state data
-}
createC : Signal.Mailbox a -> a -> Cursor a a
createC mailbox state =
  { receiver = mailbox.address
  , state = state
  , lens = idL
  , signal = mailbox.signal
  }


{-| Set cursor value
-}
setC : Cursor a b -> b -> Signal.Message
setC c1 v =
  Signal.message c1.receiver (Focus.set c1.lens v c1.state)


{-| Update cursor value via f
-}
updateC : Cursor a b -> (b -> b) -> Signal.Message
updateC c1 f =
  Signal.message c1.receiver (Focus.update c1.lens f c1.state)


{-| Get cursor value
-}
getC : Cursor a b -> b
getC c1 =
  Focus.get c1.lens c1.state


{-| Create render loop for cursor
-}
drawC : a -> (Cursor a a -> b) -> Signal.Signal b
drawC state view =
  let
    mailbox =
      Signal.mailbox state

    cursorC_ =
      createC mailbox
  in
    Signal.map (cursorC_ >> view) mailbox.signal



-- Signal.map
--   (\x ->
--     let
--       cc =
--         { cursorC_ | state = x }
--     in
--       view cc
--   )
--   mailbox.signal
