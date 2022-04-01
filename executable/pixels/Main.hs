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
  , mouse ∷ Point V2 Int
  , events ∷ [Event]
  , time ∷ Float
  , recursion ∷ IO α
  }

runSdl ∷ ReaderT (SdlState α) IO α → IO α
runSdl action = do
  initializeAll
  window ← createWindow "…" defaultWindow
  renderer ← createRenderer window (-1) defaultRenderer {rendererType = AcceleratedVSyncRenderer}
  result ← fix \ recursion → do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    present renderer
    mouse ← (fmap ∘ fmap) fromIntegral getAbsoluteMouseLocation
    events ← pollEvents
    time ← time
    runReaderT action SdlState {..}
  destroyWindow window
  return result

recurse ∷ ReaderT (SdlState α) IO α
recurse = do
  SdlState {..} ← ask
  liftIO recursion

main ∷ IO ( )
main = runSdl do
  SdlState {..} ← ask
  liftIO do print events
  liftIO do print mouse
  liftIO do print time
  recurse
  -- case event of
  --   Keyboard Pressed KeycodeQ → return ( )
  --   _ → recurse
