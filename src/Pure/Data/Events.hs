{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, OverloadedStrings,
   FlexibleContexts, ScopedTypeVariables #-}
module Pure.Data.Events where

import Pure.Data.Default
import qualified Pure.Data.View as V (Listener(..))
import Pure.Data.View hiding (On)
import Pure.Data.View.Patterns
import Pure.Data.Txt (Txt)
import Pure.Data.Lifted ((.#),Evt(..),Options(..),JSV,IsNode(..))

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Coerce
import Data.Foldable (traverse_)

--------------------------------------------------------------------------------
-- Core Listener Patterns

pattern On :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
pattern On ev f a <- Listener (V.On ev ElementTarget _ f _) a where
  On ev f a = Listener (V.On ev ElementTarget def f (return ())) a

pattern OnWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
pattern OnWith opts ev f a <- Listener (V.On ev ElementTarget opts f _) a where
  OnWith opts ev f a = Listener (V.On ev ElementTarget opts f (return ())) a

pattern OnDoc :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
pattern OnDoc ev f a <- Listener (V.On ev DocumentTarget _ f _) a where
  OnDoc ev f a = Listener (V.On ev DocumentTarget def f (return ())) a

pattern OnDocWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
pattern OnDocWith opts ev f a <- Listener (V.On ev DocumentTarget opts f _) a where
  OnDocWith opts ev f a = Listener (V.On ev DocumentTarget opts f (return ())) a

pattern OnWin :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
pattern OnWin ev f a <- Listener (V.On ev WindowTarget _ f _) a where
  OnWin ev f a = Listener (V.On ev WindowTarget def f (return ())) a

pattern OnWinWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
pattern OnWinWith opts ev f a <- Listener (V.On ev WindowTarget opts f _) a where
  OnWinWith opts ev f a = Listener (V.On ev WindowTarget opts f (return ())) a

----------------------------------------
-- Window events

pattern OnResize :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnResize f a = OnWin "resize" f a

pattern OnResizeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnResizeWith opts f a = OnWinWith opts "resize" f a

pattern OnScroll :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnScroll f a = OnWin "scroll" f a

pattern OnScrollWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnScrollWith opts f a = OnWinWith opts "scroll" f a

pattern OnClose :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClose f a = OnWin "close" f a

pattern OnCloseWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCloseWith opts f a = OnWinWith opts "close" f a

pattern OnBeforeUnload :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBeforeUnload f a = OnWin "beforeunload" f a

pattern OnBeforeUnloadWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBeforeUnloadWith opts f a = OnWinWith opts "beforeunload" f a

----------------------------------------
-- Mouse/Touch

pattern OnClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClick f a = On "click" f a

pattern OnClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnClickWith opts f a = OnWith opts "click" f a

pattern OnDoubleClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnDoubleClick f a = On "dblclick" f a

pattern OnDoubleClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnDoubleClickWith opts f a = OnWith opts "dblclick" f a

pattern OnMouseDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseDown f a = On "mousedown" f a

pattern OnMouseDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseDownWith opts f a = OnWith opts "mousedown" f a

pattern OnMouseUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseUp f a = On "mouseup" f a

pattern OnMouseUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseUpWith opts f a = OnWith opts "mouseup" f a

pattern OnTouchStart :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchStart f a = On "touchstart" f a

pattern OnTouchStartWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchStartWith opts f a = OnWith opts "touchstart" f a

pattern OnTouchEnd :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchEnd f a = On "touchend" f a

pattern OnTouchEndWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchEndWith opts f a = OnWith opts "touchend" f a

pattern OnMouseEnter :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseEnter f a = On "mouseenter" f a

pattern OnMouseEnterWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseEnterWith opts f a = OnWith opts "mouseenter" f a

pattern OnMouseLeave :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseLeave f a = On "mouseleave" f a

pattern OnMouseLeaveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseLeaveWith opts f a = OnWith opts "mouseleave" f a

pattern OnMouseOver :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOver f a = On "mouseover" f a

pattern OnMouseOverWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOverWith opts f a = OnWith opts "mouseover" f a

pattern OnMouseOut :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOut f a = On "mouseout" f a

pattern OnMouseOutWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOutWith opts f a = OnWith opts "mouseout" f a

pattern OnMouseMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseMove f a = On "mousemove" f a

pattern OnMouseMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseMoveWith opts f a = OnWith opts "mousemove" f a

pattern OnTouchMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchMove f a = On "touchmove" f a 

pattern OnTouchMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchMoveWith opts f a = OnWith opts "touchmove" f a 

pattern OnTouchCancel :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchCancel f a = On "touchcancel" f a

pattern OnTouchCancelWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchCancelWith opts f a = OnWith opts "touchcancel" f a

--------------------------------------------------------------------------------
-- Focus/Blur

pattern OnBlur :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBlur f a = On "blur" f a

pattern OnBlurWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBlurWith opts f a = OnWith opts "blur" f a

pattern OnFocus :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnFocus f a = On "focus" f a

pattern OnFocusWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnFocusWith opts f a = OnWith opts "focus" f a

--------------------------------------------------------------------------------
-- Inputs

pattern OnInput :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnInput f a = On "input" f a

pattern OnInputWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnInputWith opts f a = OnWith opts "input" f a

pattern OnChange :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnChange f a = On "change" f a

pattern OnChangeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnChangeWith opts f a = OnWith opts "change" f a

pattern OnCheck :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnCheck f a = On "change" f a

pattern OnCheckWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCheckWith opts f a = OnWith opts "change" f a

pattern OnSubmit :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnSubmit f a = On "submit" f a

pattern OnSubmitWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnSubmitWith opts f a = OnWith opts "submit" f a

--------------------------------------------------------------------------------
-- Keys

pattern OnKeyUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyUp f a = On "keyup" f a

pattern OnKeyUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyUpWith opts f a = OnWith opts "keyup" f a

pattern OnKeyDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyDown f a = On "keydown" f a

pattern OnKeyDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyDownWith opts f a = OnWith opts "keydown" f a

pattern OnKeyPress :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyPress f a = On "keypress" f a

pattern OnKeyPressWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyPressWith opts f a = OnWith opts "keypress" f a

--------------------------------------------------------------------------------
-- Event Helpers

passive :: Options
passive = Options False False True False

intercept :: Options
intercept = Options True True False True

nodefault :: Options
nodefault = Options True False False False

noprop :: Options
noprop = Options False True False False

withInput :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withInput f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

-- same as withInput
withValue :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withValue f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

withChecked :: (Bool -> IO ()) -> (Evt -> IO ())
withChecked f = traverse_ f . join . fmap (.# "checked") . (.# "target") . evtObj

key :: Evt -> Maybe Txt
key = (.# "key") . evtObj

code :: Evt -> Maybe Txt
code = (.# "code") . evtObj

clientY :: Evt -> Maybe Int
clientY = (.# "clientY") . evtObj

clientX :: Evt -> Maybe Int
clientX = (.# "clientX") . evtObj

screenY :: Evt -> Maybe Int
screenY = (.# "screenY") . evtObj

screenX :: Evt -> Maybe Int
screenX = (.# "screenX") . evtObj

movementY :: Evt -> Maybe Int
movementY = (.# "movementY") . evtObj

movementX :: Evt -> Maybe Int
movementX = (.# "movementX") . evtObj

pageY :: Evt -> Maybe Int
pageY = (.# "pageY") . evtObj

pageX :: Evt -> Maybe Int
pageX = (.# "pageX") . evtObj

shift :: Evt -> Maybe Bool
shift = (.# "shiftKey") . evtObj

alt :: Evt -> Maybe Bool
alt = (.# "altKey") . evtObj

ctrl :: Evt -> Maybe Bool
ctrl = (.# "ctrlKey") . evtObj

meta :: Evt -> Maybe Bool
meta = (.# "metaKey") . evtObj

button :: Evt -> Maybe Int
button = (.# "button") . evtObj

target :: Evt -> Maybe JSV
target = (.# "target") . evtObj

relatedTarget :: Evt -> Maybe JSV
relatedTarget = (.# "relatedTarget") . evtObj

pattern LeftButton e <- (button &&& id -> (Just 0,e))
pattern MiddleButton e <- (button &&& id -> (Just 1,e))
pattern RightButton e <- (button &&& id -> (Just 2,e))
pattern BackButton e <- (button &&& id -> (Just 3,e))
pattern ForwardButton e <- (button &&& id -> (Just 4,e))

pattern Target t e <- (target &&& id -> (Just t,e))
pattern RelatedTarget t e <- (relatedTarget &&& id -> (Just t,e))

pattern ShiftModifier o <- (shift &&& id -> (Just True,o))
pattern AltModifier o <- (alt &&& id -> (Just True,o))
pattern CtrlModifier o <- (ctrl &&& id -> (Just True,o))
pattern MetaModifier o <- (meta &&& id -> (Just True,o))

pattern ClientY y e <- (clientY &&& id -> (Just y,e))
pattern ClientX x e <- (clientX &&& id -> (Just x,e))
pattern ScreenY y e <- (screenY &&& id -> (Just y,e))
pattern ScreenX x e <- (screenX &&& id -> (Just x,e))
pattern MovementY y e <- (movementY &&& id -> (Just y,e))
pattern MovementX x e <- (movementX &&& id -> (Just x,e))
pattern PageY y e <- (pageY &&& id -> (Just y,e))
pattern PageX x e <- (pageX &&& id -> (Just x,e))

pattern Key0 e <- (key &&& id -> (Just "0",e))
pattern Key1 e <- (key &&& id -> (Just "1",e))
pattern Key2 e <- (key &&& id -> (Just "2",e))
pattern Key3 e <- (key &&& id -> (Just "3",e))
pattern Key4 e <- (key &&& id -> (Just "4",e))
pattern Key5 e <- (key &&& id -> (Just "5",e))
pattern Key6 e <- (key &&& id -> (Just "6",e))
pattern Key7 e <- (key &&& id -> (Just "7",e))
pattern Key8 e <- (key &&& id -> (Just "8",e))
pattern Key9 e <- (key &&& id -> (Just "9",e))

pattern KeyA e <- (key &&& id -> (Just "KeyA",e))
pattern KeyB e <- (key &&& id -> (Just "KeyB",e))
pattern KeyC e <- (key &&& id -> (Just "KeyC",e))
pattern KeyD e <- (key &&& id -> (Just "KeyD",e))
pattern KeyE e <- (key &&& id -> (Just "KeyE",e))
pattern KeyF e <- (key &&& id -> (Just "KeyF",e))
pattern KeyG e <- (key &&& id -> (Just "KeyG",e))
pattern KeyH e <- (key &&& id -> (Just "KeyH",e))
pattern KeyI e <- (key &&& id -> (Just "KeyI",e))
pattern KeyJ e <- (key &&& id -> (Just "KeyJ",e))
pattern KeyK e <- (key &&& id -> (Just "KeyK",e))
pattern KeyL e <- (key &&& id -> (Just "KeyL",e))
pattern KeyM e <- (key &&& id -> (Just "KeyM",e))
pattern KeyN e <- (key &&& id -> (Just "KeyN",e))
pattern KeyO e <- (key &&& id -> (Just "KeyO",e))
pattern KeyP e <- (key &&& id -> (Just "KeyP",e))
pattern KeyQ e <- (key &&& id -> (Just "KeyQ",e))
pattern KeyR e <- (key &&& id -> (Just "KeyR",e))
pattern KeyS e <- (key &&& id -> (Just "KeyS",e))
pattern KeyT e <- (key &&& id -> (Just "KeyT",e))
pattern KeyU e <- (key &&& id -> (Just "KeyU",e))
pattern KeyV e <- (key &&& id -> (Just "KeyV",e))
pattern KeyW e <- (key &&& id -> (Just "KeyW",e))
pattern KeyX e <- (key &&& id -> (Just "KeyX",e))
pattern KeyY e <- (key &&& id -> (Just "KeyY",e))
pattern KeyZ e <- (key &&& id -> (Just "KeyZ",e))

pattern OpenParenthesis   e <- (key &&& id -> (Just "(",e))
pattern CloseParenthesis  e <- (key &&& id -> (Just ")",e))
pattern Exclamation       e <- (key &&& id -> (Just "!",e))
pattern At                e <- (key &&& id -> (Just "@",e))
pattern NumberSign        e <- (key &&& id -> (Just "#",e))
pattern Dollar            e <- (key &&& id -> (Just "$",e))
pattern Percent           e <- (key &&& id -> (Just "%",e))
pattern Caret             e <- (key &&& id -> (Just "^",e))
pattern Ampersand         e <- (key &&& id -> (Just "&",e))
pattern Asterisk          e <- (key &&& id -> (Just "*",e))
pattern Underscore        e <- (key &&& id -> (Just "_",e))
pattern Plus              e <- (key &&& id -> (Just "+",e))
pattern VerticalBar       e <- (key &&& id -> (Just "|",e))
pattern CurlyBracketLeft  e <- (key &&& id -> (Just "{",e))
pattern CurlyBracketRight e <- (key &&& id -> (Just "}",e))
pattern QuestionMark      e <- (key &&& id -> (Just "?",e))
pattern ForwardSlash      e <- (key &&& id -> (Just "/",e))
pattern Tilde             e <- (key &&& id -> (Just "~",e))
pattern Grave             e <- (key &&& id -> (Just "`",e))
pattern Colon             e <- (key &&& id -> (Just ":",e))
pattern Semicolon         e <- (key &&& id -> (Just ";",e))
pattern Comma             e <- (key &&& id -> (Just ",",e))
pattern Period            e <- (key &&& id -> (Just ".",e))
pattern Quote             e <- (key &&& id -> (Just "'",e))
pattern DoubleQuote       e <- (key &&& id -> (Just "\"",e))
pattern BracketLeft       e <- (key &&& id -> (Just "[",e))
pattern BracketRight      e <- (key &&& id -> (Just "]",e))
pattern Backslash         e <- (key &&& id -> (Just "\\",e))
pattern Minus             e <- (key &&& id -> (Just "-",e))
pattern Equal             e <- (key &&& id -> (Just "=",e))

pattern Alt          e <- (key &&& id -> (Just "Alt",e))
pattern AltGr        e <- (key &&& id -> (Just "AltGraph",e))
pattern CapsLock     e <- (key &&& id -> (Just "CapsLock",e))
pattern Control      e <- (key &&& id -> (Just "Control",e))
pattern Fn           e <- (key &&& id -> (Just "Fn",e))
pattern FnLock       e <- (key &&& id -> (Just "FnLock",e))
pattern Hyper        e <- (key &&& id -> (Just "Hyper",e))
pattern OS           e <- (key &&& id -> (Just "Meta",e))
pattern NumLock      e <- (key &&& id -> (Just "NumLock",e))
pattern ScrollLock   e <- (key &&& id -> (Just "ScrollLock",e))
pattern Shift        e <- (key &&& id -> (Just "Shift",e))
pattern Super        e <- (key &&& id -> (Just "Super",e))

pattern Enter       e <- (key &&& id -> (Just "Enter",e))
pattern Space       e <- (key &&& id -> (Just " ",e))
pattern Tab         e <- (key &&& id -> (Just "Tab",e))

pattern ArrowDown   e <- (key &&& id -> (Just "ArrowDown",e))
pattern ArrowLeft   e <- (key &&& id -> (Just "ArrowLeft",e))
pattern ArrowRight  e <- (key &&& id -> (Just "ArrowRight",e))
pattern ArrowUp     e <- (key &&& id -> (Just "ArrowUp",e))
pattern End         e <- (key &&& id -> (Just "End",e))
pattern Home        e <- (key &&& id -> (Just "Home",e))
pattern PageDown    e <- (key &&& id -> (Just "PageDown",e))
pattern PageUp      e <- (key &&& id -> (Just "PageUp",e))

pattern Backspace   e <- (key &&& id -> (Just "Backspace",e))
pattern Clear       e <- (key &&& id -> (Just "Clear",e))
pattern Copy        e <- (key &&& id -> (Just "Copy",e))
pattern CrSel       e <- (key &&& id -> (Just "CrSel",e))
pattern Cut         e <- (key &&& id -> (Just "Cut",e))
pattern Delete      e <- (key &&& id -> (Just "Delete",e))
pattern EraseEof    e <- (key &&& id -> (Just "EraseEof",e))
pattern ExSel       e <- (key &&& id -> (Just "ExSel",e))
pattern Insert      e <- (key &&& id -> (Just "Insert",e))
pattern Paste       e <- (key &&& id -> (Just "Paste",e))
pattern Redo        e <- (key &&& id -> (Just "Redo",e))
pattern Undo        e <- (key &&& id -> (Just "Undo",e))

pattern Accept      e <- (key &&& id -> (Just "Accept",e))
pattern Again       e <- (key &&& id -> (Just "Again",e))
pattern Attn        e <- (key &&& id -> (Just "Attn",e))
pattern Cancel      e <- (key &&& id -> (Just "Cancel",e))
pattern ContextMenu e <- (key &&& id -> (Just "ContextMenu",e))
pattern Escape      e <- (key &&& id -> (Just "Escape",e))
pattern Excecute    e <- (key &&& id -> (Just "Execute",e))
pattern Find        e <- (key &&& id -> (Just "Find",e))
pattern Finish      e <- (key &&& id -> (Just "Finish",e))
pattern Help        e <- (key &&& id -> (Just "Help",e))
pattern Pause       e <- (key &&& id -> (Just "Pause",e))
pattern Play        e <- (key &&& id -> (Just "Play",e))
pattern Props       e <- (key &&& id -> (Just "Props",e))
pattern Select      e <- (key &&& id -> (Just "Select",e))
pattern ZoomIn      e <- (key &&& id -> (Just "ZoomIn",e))
pattern ZoomOut     e <- (key &&& id -> (Just "ZoomOut",e))

pattern F1 e <- (key &&& id -> (Just "F1",e))
pattern F2 e <- (key &&& id -> (Just "F2",e))
pattern F3 e <- (key &&& id -> (Just "F3",e))
pattern F4 e <- (key &&& id -> (Just "F4",e))
pattern F5 e <- (key &&& id -> (Just "F5",e))
pattern F6 e <- (key &&& id -> (Just "F6",e))
pattern F7 e <- (key &&& id -> (Just "F7",e))
pattern F8 e <- (key &&& id -> (Just "F8",e))
pattern F9 e <- (key &&& id -> (Just "F9",e))
pattern F10 e <- (key &&& id -> (Just "F10",e))
pattern F11 e <- (key &&& id -> (Just "F11",e))
pattern F12 e <- (key &&& id -> (Just "F12",e))
pattern F13 e <- (key &&& id -> (Just "F13",e))
pattern F14 e <- (key &&& id -> (Just "F14",e))
pattern F15 e <- (key &&& id -> (Just "F15",e))
pattern F16 e <- (key &&& id -> (Just "F16",e))
pattern F17 e <- (key &&& id -> (Just "F17",e))
pattern F18 e <- (key &&& id -> (Just "F18",e))
pattern F19 e <- (key &&& id -> (Just "F19",e))
pattern F20 e <- (key &&& id -> (Just "F20",e))
