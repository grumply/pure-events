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
passive = Options False False True

intercept :: Options
intercept = Options True True False

nodefault :: Options
nodefault = Options True False False

noprop :: Options
noprop = Options False True False

withInput :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withInput f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

-- same as withInput
withValue :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withValue f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

withChecked :: (Bool -> IO ()) -> (Evt -> IO ())
withChecked f = traverse_ f . join . fmap (.# "checked") . (.# "target") . evtObj

keyCode :: Evt -> Maybe Int
keyCode = (.# "keyCode") . evtObj

clientY :: Evt -> Maybe Int
clientY = (.# "clientY") . evtObj

clientX :: Evt -> Maybe Int
clientX = (.# "clientX") . evtObj

screenY :: Evt -> Maybe Int
screenY = (.# "screenY") . evtObj

screenX :: Evt -> Maybe Int
screenX = (.# "screenX") . evtObj

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

relatedTarget :: Evt -> Maybe JSV
relatedTarget = (.# "relatedTarget") . evtObj

pattern LeftButton e <- (button &&& id -> (Just 0,e))
pattern MiddleButton e <- (button &&& id -> (Just 1,e))
pattern RightButton e <- (button &&& id -> (Just 2,e))
pattern BackButton e <- (button &&& id -> (Just 3,e))
pattern ForwardButton e <- (button &&& id -> (Just 4,e))

pattern RelatedTarget t <- (relatedTarget -> Just t)

pattern ShiftKey o <- (shift &&& id -> (Just True,o))
pattern AltKey o <- (alt &&& id -> (Just True,o))
pattern CtrlKey o <- (ctrl &&& id -> (Just True,o))
pattern MetaKey o <- (meta &&& id -> (Just True,o))

pattern ClientY y e <- (clientY &&& id -> (Just y,e))
pattern ClientX x e <- (clientX &&& id -> (Just x,e))
pattern ScreenY y e <- (screenY &&& id -> (Just y,e))
pattern ScreenX x e <- (screenX &&& id -> (Just x,e))

pattern Key0 <- (keyCode -> Just 48)
pattern Key1 <- (keyCode -> Just 49)
pattern Key2 <- (keyCode -> Just 50)
pattern Key3 <- (keyCode -> Just 51)
pattern Key4 <- (keyCode -> Just 52)
pattern Key5 <- (keyCode -> Just 53)
pattern Key6 <- (keyCode -> Just 54)
pattern Key7 <- (keyCode -> Just 55)
pattern Key8 <- (keyCode -> Just 56)
pattern Key9 <- (keyCode -> Just 57)

pattern Keya <- (keyCode -> Just 97)
pattern Keyb <- (keyCode -> Just 98)
pattern Keyc <- (keyCode -> Just 99)
pattern Keyd <- (keyCode -> Just 100)
pattern Keye <- (keyCode -> Just 101)
pattern Keyf <- (keyCode -> Just 102)
pattern Keyg <- (keyCode -> Just 103)
pattern Keyh <- (keyCode -> Just 104)
pattern Keyi <- (keyCode -> Just 105)
pattern Keyj <- (keyCode -> Just 106)
pattern Keyk <- (keyCode -> Just 107)
pattern Keyl <- (keyCode -> Just 108)
pattern Keym <- (keyCode -> Just 109)
pattern Keyn <- (keyCode -> Just 110)
pattern Keyo <- (keyCode -> Just 111)
pattern Keyp <- (keyCode -> Just 112)
pattern Keyq <- (keyCode -> Just 113)
pattern Keyr <- (keyCode -> Just 114)
pattern Keys <- (keyCode -> Just 115)
pattern Keyt <- (keyCode -> Just 116)
pattern Keyu <- (keyCode -> Just 117)
pattern Keyv <- (keyCode -> Just 118)
pattern Keyw <- (keyCode -> Just 119)
pattern Keyx <- (keyCode -> Just 120)
pattern Keyy <- (keyCode -> Just 121)
pattern Keyz <- (keyCode -> Just 122)

pattern KeyA <- (keyCode -> Just 65)
pattern KeyB <- (keyCode -> Just 66)
pattern KeyC <- (keyCode -> Just 67)
pattern KeyD <- (keyCode -> Just 68)
pattern KeyE <- (keyCode -> Just 69)
pattern KeyF <- (keyCode -> Just 70)
pattern KeyG <- (keyCode -> Just 71)
pattern KeyH <- (keyCode -> Just 72)
pattern KeyI <- (keyCode -> Just 73)
pattern KeyJ <- (keyCode -> Just 74)
pattern KeyK <- (keyCode -> Just 75)
pattern KeyL <- (keyCode -> Just 76)
pattern KeyM <- (keyCode -> Just 77)
pattern KeyN <- (keyCode -> Just 78)
pattern KeyO <- (keyCode -> Just 79)
pattern KeyP <- (keyCode -> Just 80)
pattern KeyQ <- (keyCode -> Just 81)
pattern KeyR <- (keyCode -> Just 82)
pattern KeyS <- (keyCode -> Just 83)
pattern KeyT <- (keyCode -> Just 84)
pattern KeyU <- (keyCode -> Just 85)
pattern KeyV <- (keyCode -> Just 86)
pattern KeyW <- (keyCode -> Just 87)
pattern KeyX <- (keyCode -> Just 88)
pattern KeyY <- (keyCode -> Just 89)
pattern KeyZ <- (keyCode -> Just 90)

pattern OpenParenthesis <- ShiftKey (keyCode -> Just 40)
pattern CloseParenthesis <- ShiftKey (keyCode -> Just 41)
pattern Exclamation <- ShiftKey (keyCode -> Just 33)
pattern At <- ShiftKey (keyCode -> Just 64)
pattern NumberSign <- ShiftKey (keyCode -> Just 35)
pattern Dollar <- ShiftKey (keyCode -> Just 36)
pattern Percent <- ShiftKey (keyCode -> Just 37)
pattern Caret <- ShiftKey (keyCode -> Just 94)
pattern Ampersand <- ShiftKey (keyCode -> Just 38)
pattern Asterisk <- ShiftKey (keyCode -> Just 42)
pattern Underscore <- ShiftKey (keyCode -> Just 95)
pattern Plus <- ShiftKey (keyCode -> Just 43)
pattern VerticalBar <- ShiftKey (keyCode -> Just 124)
pattern CurlyBracketLeft <- ShiftKey (keyCode -> Just 123)
pattern CurlyBracketRight <- ShiftKey (keyCode -> Just 125)
pattern QuestionMark <- ShiftKey (keyCode -> Just 63)
pattern FullStop <- (keyCode -> Just 46)
pattern ForwardSlash <- (keyCode -> Just 47)
pattern Tilde <- (keyCode -> Just 96)
pattern Grave <- ShiftKey (keyCode -> Just 126)
pattern Colon <- ShiftKey (keyCode -> Just 58)
pattern Semicolon <- (keyCode -> Just 59)
pattern Comma <- (keyCode -> Just 44)
pattern Period <- (keyCode -> Just 46)
pattern Quote <- (keyCode -> Just 39)
pattern DoubleQuote <- ShiftKey (keyCode -> Just 34)
pattern BracketLeft <- (keyCode -> Just 91)
pattern BracketRight <- (keyCode -> Just 93)
pattern Backslash <- (keyCode -> Just 47)
pattern Minus <- (keyCode -> Just 45)
pattern Equal <- (keyCode -> Just 61)

pattern KeyAlt <- (keyCode -> Just 18)
pattern KeyCapsLock <- (keyCode -> Just 20)
pattern KeyControl <- (keyCode -> Just 17)
pattern KeyOSLeft <- (keyCode -> Just 91)
pattern KeyOSRight <- (keyCode -> Just 92)
pattern KeyShift <- (keyCode -> Just 16)

pattern ContextMenu <- (keyCode -> Just 93)
pattern Enter <- (keyCode -> Just 13)
pattern Space <- (keyCode -> Just 32)
pattern Tab <- (keyCode -> Just 9)
pattern Delete <- (keyCode -> Just 46)
pattern EndKey <- (keyCode -> Just 35)
pattern Home <- (keyCode -> Just 36)
pattern Insert <- (keyCode -> Just 45)
pattern PageDown <- (keyCode -> Just 34)
pattern PageUp <- (keyCode -> Just 33)
pattern ArrowDown <- (keyCode -> Just 40)
pattern ArrowLeft <- (keyCode -> Just 37)
pattern ArrowRight <- (keyCode -> Just 39)
pattern ArrowUp <- (keyCode -> Just 38)
pattern Escape <- (keyCode -> Just 27)
pattern PrintScreen <- (keyCode -> Just 44)
pattern ScrollLock <- (keyCode -> Just 145)
pattern Pause <- (keyCode -> Just 19)

pattern F1 <- (keyCode -> Just 112)
pattern F2 <- (keyCode -> Just 113)
pattern F3 <- (keyCode -> Just 114)
pattern F4 <- (keyCode -> Just 115)
pattern F5 <- (keyCode -> Just 116)
pattern F6 <- (keyCode -> Just 117)
pattern F7 <- (keyCode -> Just 118)
pattern F8 <- (keyCode -> Just 119)
pattern F9 <- (keyCode -> Just 120)
pattern F10 <- (keyCode -> Just 121)
pattern F11 <- (keyCode -> Just 122)
pattern F12 <- (keyCode -> Just 123)
pattern F13 <- (keyCode -> Just 124)
pattern F14 <- (keyCode -> Just 125)
pattern F15 <- (keyCode -> Just 126)
pattern F16 <- (keyCode -> Just 127)
pattern F17 <- (keyCode -> Just 128)
pattern F18 <- (keyCode -> Just 129)
pattern F19 <- (keyCode -> Just 130)
pattern F20 <- (keyCode -> Just 131)
pattern F21 <- (keyCode -> Just 132)
pattern F22 <- (keyCode -> Just 133)
pattern F23 <- (keyCode -> Just 134)
pattern F24 <- (keyCode -> Just 135)

pattern NumLock <- (keyCode -> Just 144)
pattern Numpad0 <- (keyCode -> Just 96)
pattern Numpad1 <- (keyCode -> Just 97)
pattern Numpad2 <- (keyCode -> Just 98)
pattern Numpad3 <- (keyCode -> Just 99)
pattern Numpad4 <- (keyCode -> Just 100)
pattern Numpad5 <- (keyCode -> Just 101)
pattern Numpad6 <- (keyCode -> Just 102)
pattern Numpad7 <- (keyCode -> Just 103)
pattern Numpad8 <- (keyCode -> Just 104)
pattern Numpad9 <- (keyCode -> Just 105)
pattern NumpadAdd <- (keyCode -> Just 107)
pattern NumpadComma <- (keyCode -> Just 194)
pattern NumpadDecimal <- (keyCode -> Just 110)
pattern NumpadDivide <- (keyCode -> Just 111)
pattern NumpadEnter <- (keyCode -> Just 13)
pattern NumpadEqual <- (keyCode -> Just 12)
pattern NumpadMultiply <- (keyCode -> Just 106)
pattern NumpadSubtract <- (keyCode -> Just 109)