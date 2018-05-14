{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, OverloadedStrings #-}
module Pure.Data.Events where

import Pure.Data.Default
import qualified Pure.Data.View as V (Listener(..))
import Pure.Data.View hiding (On)
import Pure.Data.View.Patterns
import Pure.Data.Txt (Txt)
import Pure.Data.Lifted ((.#))

import Control.Monad (join)
import Data.Foldable (traverse_)

pattern On :: Txt -> (Evt -> IO ()) -> Listener
pattern On ev f <- (V.On ev ElementTarget _ f _) where
  On ev f = V.On ev ElementTarget def f (return ())

pattern OnDoc :: Txt -> (Evt -> IO ()) -> Listener
pattern OnDoc ev f <- (V.On ev DocumentTarget _ f _) where
  OnDoc ev f = V.On ev DocumentTarget def f (return ())

pattern OnWin :: Txt -> (Evt -> IO ()) -> Listener
pattern OnWin ev f <- (V.On ev WindowTarget _ f _) where
  OnWin ev f = V.On ev WindowTarget def f (return ())

preventedDefault :: Listener -> (Bool,Listener)
preventedDefault f@(V.On _ _ (Options True _ _) _ _) = (True,f)

preventDefault :: Listener -> Listener
preventDefault (V.On ev t os f s) = V.On ev t (os { preventDef = True, passive = False }) f s

pattern PreventDefault :: Listener -> Listener
pattern PreventDefault f <- (preventedDefault -> (True,f)) where
  PreventDefault f = preventDefault f

stoppedPropagation :: Listener -> (Bool,Listener)
stoppedPropagation f@(V.On _ _ (Options _ True _) _ _) = (True,f)

stopPropagation :: Listener -> Listener
stopPropagation (V.On ev t os f s) = V.On ev t (os { stopProp = True }) f s

pattern StopPropagation :: Listener -> Listener
pattern StopPropagation f <- (stoppedPropagation -> (True,f)) where
  StopPropagation f = stopPropagation f

intercepted :: Listener -> (Bool,Listener)
intercepted f@(V.On _ _ (Options True True _) _ _) = (True,f)

intercept :: Listener -> Listener
intercept (V.On ev t os f s) = V.On ev t (os { preventDef = True, stopProp = True, passive = False }) f s

pattern Intercept :: Listener -> Listener
pattern Intercept f <- (intercepted -> (True,f)) where
  Intercept f = intercept f

----------------------------------------
-- Window events

pattern OnResize f <- (OnWin "resize" f) where
  OnResize f = OnWin "resize" f

pattern OnScroll f <- (OnWin "scroll" f) where
  OnScroll f = OnWin "scroll" f

pattern OnClose f <- (OnWin "close" f) where
  OnClose f = OnWin "close" f

pattern OnBeforeUnload f <- (OnWin "beforeunload" f) where
  OnBeforeUnload f = OnWin "beforeunload" f

----------------------------------------
-- Element events

pattern OnClick f <- (On "click" f) where
  OnClick f = On "click" f

pattern OnDoubleClick f <- (On "dblclick" f) where
  OnDoubleClick f = On "dblclick" f

pattern OnMouseDown f <- (On "mousedown" f) where
  OnMouseDown f = On "mousedown" f

pattern OnMouseUp f <- (On "mouseup" f) where
  OnMouseUp f = On "mouseup" f

pattern OnTouchStart f <- (On "touchstart" f) where
  OnTouchStart f = On "touchstart" f

pattern OnTouchEnd f <- (On "touchend" f) where
  OnTouchEnd f = On "touchend" f

pattern OnMouseEnter f <- (On "mouseenter" f) where
  OnMouseEnter f = On "mouseenter" f

pattern OnMouseLeave f <- (On "mouseleave" f) where
  OnMouseLeave f = On "mouseleave" f

pattern OnMouseOver f <- (On "mouseover" f) where
  OnMouseOver f = On "mouseover" f

pattern OnMouseOut f <- (On "mouseout" f) where
  OnMouseOut f = On "mouseout" f

pattern OnMouseMove f <- (On "mousemove" f) where
  OnMouseMove f = On "mousemove" f

pattern OnTouchMove f <- (On "touchmove" f)  where
  OnTouchMove f = On "touchmove" f

pattern OnTouchCancel f <- (On "touchcancel" f) where
  OnTouchCancel f = On "touchcancel" f

pattern OnSubmit f <- (On "submit" f) where
  OnSubmit f = On "submit" f

pattern OnBlur f <- (On "blur" f) where
  OnBlur f = On "blur" f

pattern OnFocus f <- (On "focus" f) where
  OnFocus f = On "focus" f

pattern OnKeyUp f <- (On "keyup" f) where
  OnKeyUp f = On "keyup" f

pattern OnKeyDown f <- (On "keydown" f) where
  OnKeyDown f = On "keydown" f

pattern OnKeyPress f <- (On "keypress" f) where
  OnKeyPress f = On "keypress" f

pattern OnInput :: (Evt -> IO ()) -> Listener
pattern OnInput f = On "input" f

withInput :: (Txt -> IO ()) -> (Evt -> IO ())
withInput f = traverse_ f . join . fmap (.# "value") . (.# "target") . evtObj

pattern OnChange :: (Evt -> IO ()) -> Listener
pattern OnChange f = On "change" f

withValue :: (Txt -> IO ()) -> (Evt -> IO ())
withValue f = traverse_ f . join . fmap (.# "value") . (.# "target") . evtObj

pattern OnCheck :: (Evt -> IO ()) -> Listener
pattern OnCheck f = On "change" f

withChecked :: (Bool -> IO ()) -> (Evt -> IO ())
withChecked f = traverse_ f . join . fmap (.# "checked") . (.# "target") . evtObj

--------------------------------------------------------------------------------
-- Keys

keyCode :: Evt -> Maybe Int
keyCode = (.# "keyCode") . evtObj

pattern Digit0 <- (keyCode -> Just 48)
pattern Digit1 <- (keyCode -> Just 49)
pattern Digit2 <- (keyCode -> Just 50)
pattern Digit3 <- (keyCode -> Just 51)
pattern Digit4 <- (keyCode -> Just 52)
pattern Digit5 <- (keyCode -> Just 53)
pattern Digit6 <- (keyCode -> Just 54)
pattern Digit7 <- (keyCode -> Just 55)
pattern Digit8 <- (keyCode -> Just 56)
pattern Digit9 <- (keyCode -> Just 57)

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

shiftModifier o = ((.# "shiftKey") (evtObj o),o)
pattern ShiftKey o <- (shiftModifier -> (Just True,o))

altModifier o = ((.# "altKey") (evtObj o),o)
pattern AltKey o <- (altModifier -> (Just True,o))

ctrlModifier o = ((.# "ctrlKey") (evtObj o),o)
pattern CtrlKey o <- (ctrlModifier -> (Just True,o))

metaModifier o = ((.# "metaKey") (evtObj o),o)
pattern MetaKey o <- (metaModifier -> (Just True,o))

