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
  , recursion ∷ IO α
  }

runSdl ∷ ReaderT (SdlState α) IO α → IO α
runSdl action = do
  initializeAll
  window ← createWindow "…" defaultWindow
  renderer ← createRenderer window (-1) defaultRenderer
  result ← fix \ recursion → do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    present renderer
    runReaderT action SdlState {..}
  destroyWindow window
  return result

recurse ∷ ReaderT (SdlState α) IO α
recurse = do
  SdlState {..} ← ask
  liftIO recursion

main ∷ IO ( )
main = runSdl do
  event ← waitEvent
  case event of
    Keyboard Pressed KeycodeQ → return ( )
    _ → recurse
