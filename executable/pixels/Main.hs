module Main where

import Prelude.Unicode
import SDL
import SDL.Event
import Linear (V4(..))
import Control.Monad
import Data.Function
import Control.Monad.Reader
import Data.Word

pattern Keyboard ∷ InputMotion → Keycode → Event
pattern Keyboard keyboardEventKeyMotion keysymKeycode ← Event
  {eventPayload = KeyboardEvent KeyboardEventData {keyboardEventKeysym = Keysym {..}, ..}}

data SdlState α = SdlState
  { window ∷ Window
  , renderer ∷ Renderer
  }

runSdl ∷ (SdlState α → IO α → IO α) → IO α
runSdl action = do
  initializeAll
  window ← createWindow "…" defaultWindow
  renderer ← createRenderer window (-1) defaultRenderer
  result ← fix \ recurse → do
    clear renderer
    present renderer
    action SdlState {..} recurse
  destroyWindow window
  return result

setDrawColor ∷ V4 Word8 → Renderer → IO ( )
setDrawColor color renderer = do
  rendererDrawColor renderer $= color
  return ( )

main ∷ IO ( )
main = runSdl \ SdlState {..} recurse → do
  setDrawColor (V4 0 0 255 255) renderer
  event ← waitEvent
  case event of
    Keyboard Pressed KeycodeQ → return ( )
    _ → recurse
