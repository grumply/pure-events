{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, OverloadedStrings,
   FlexibleContexts, ScopedTypeVariables #-}
module Pure.Data.Events where

import Pure.Data.Default
import qualified Pure.Data.View as V (Listener(..))
import Pure.Data.View hiding (On)
import Pure.Data.View.Patterns
import Pure.Data.Txt (Txt)
import Pure.Data.Lifted ((.#),Evt(..),Options(..),JSV)

import Control.Monad (join)
import Data.Coerce
import Data.Foldable (traverse_)

pattern On :: Txt -> (Evt -> IO ()) -> Listener
pattern On ev f <- (V.On ev ElementTarget _ f _) where
  On ev f = V.On ev ElementTarget def f (return ())

pattern OnWith :: Options -> Txt -> (Evt -> IO ()) -> Listener
pattern OnWith opts ev f <- (V.On ev ElementTarget opts f _) where
  OnWith opts ev f = V.On ev ElementTarget opts f (return ())

pattern OnDoc :: Txt -> (Evt -> IO ()) -> Listener
pattern OnDoc ev f <- (V.On ev DocumentTarget _ f _) where
  OnDoc ev f = V.On ev DocumentTarget def f (return ())

pattern OnDocWith :: Options -> Txt -> (Evt -> IO ()) -> Listener
pattern OnDocWith opts ev f <- (V.On ev DocumentTarget opts f _) where
  OnDocWith opts ev f = V.On ev DocumentTarget opts f (return ())

pattern OnWin :: Txt -> (Evt -> IO ()) -> Listener
pattern OnWin ev f <- (V.On ev WindowTarget _ f _) where
  OnWin ev f = V.On ev WindowTarget def f (return ())

pattern OnWinWith :: Options -> Txt -> (Evt -> IO ()) -> Listener
pattern OnWinWith opts ev f <- (V.On ev WindowTarget opts f _) where
  OnWinWith opts ev f = V.On ev WindowTarget opts f (return ())

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

pattern OnResize :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnResize f a = Listener (OnWin "resize" f) a

pattern OnResizeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnResizeWith opts f a = Listener (OnWinWith opts "resize" f) a

pattern OnScroll :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnScroll f a = Listener (OnWin "scroll" f) a

pattern OnScrollWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnScrollWith opts f a = Listener (OnWinWith opts "scroll" f) a

pattern OnClose :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClose f a = Listener (OnWin "close" f) a

pattern OnCloseWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCloseWith opts f a = Listener (OnWinWith opts "close" f) a

pattern OnBeforeUnload :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBeforeUnload f a = Listener (OnWin "beforeunload" f) a

pattern OnBeforeUnloadWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBeforeUnloadWith opts f a = Listener (OnWinWith opts "beforeunload" f) a

----------------------------------------
-- Element events

pattern OnClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClick f a = Listener (On "click" f) a

pattern OnClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnClickWith opts f a = Listener (OnWith opts "click" f) a

pattern OnDoubleClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnDoubleClick f a = Listener (On "dblclick" f) a

pattern OnDoubleClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnDoubleClickWith opts f a = Listener (OnWith opts "dblclick" f) a

pattern OnMouseDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseDown f a = Listener (On "mousedown" f) a

pattern OnMouseDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseDownWith opts f a = Listener (OnWith opts "mousedown" f) a

pattern OnMouseUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseUp f a = Listener (On "mouseup" f) a

pattern OnMouseUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseUpWith opts f a = Listener (OnWith opts "mouseup" f) a

pattern OnTouchStart :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchStart f a = Listener (On "touchstart" f) a

pattern OnTouchStartWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchStartWith opts f a = Listener (OnWith opts "touchstart" f) a

pattern OnTouchEnd :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchEnd f a = Listener (On "touchend" f) a

pattern OnTouchEndWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchEndWith opts f a = Listener (OnWith opts "touchend" f) a

pattern OnMouseEnter :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseEnter f a = Listener (On "mouseenter" f) a

pattern OnMouseEnterWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseEnterWith opts f a = Listener (OnWith opts "mouseenter" f) a

pattern OnMouseLeave :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseLeave f a = Listener (On "mouseleave" f) a

pattern OnMouseLeaveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseLeaveWith opts f a = Listener (OnWith opts "mouseleave" f) a

pattern OnMouseOver :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOver f a = Listener (On "mouseover" f) a

pattern OnMouseOverWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOverWith opts f a = Listener (OnWith opts "mouseover" f) a

pattern OnMouseOut :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOut f a = Listener (On "mouseout" f) a

pattern OnMouseOutWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOutWith opts f a = Listener (OnWith opts "mouseout" f) a

pattern OnMouseMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseMove f a = Listener (On "mousemove" f) a

pattern OnMouseMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseMoveWith opts f a = Listener (OnWith opts "mousemove" f) a

pattern OnTouchMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchMove f a = Listener (On "touchmove" f) a 

pattern OnTouchMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchMoveWith opts f a = Listener (OnWith opts "touchmove" f) a 

pattern OnTouchCancel :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchCancel f a = Listener (On "touchcancel" f) a

pattern OnTouchCancelWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchCancelWith opts f a = Listener (OnWith opts "touchcancel" f) a

pattern OnSubmit :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnSubmit f a = Listener (On "submit" f) a

pattern OnSubmitWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnSubmitWith opts f a = Listener (OnWith opts "submit" f) a

pattern OnBlur :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBlur f a = Listener (On "blur" f) a

pattern OnBlurWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBlurWith opts f a = Listener (OnWith opts "blur" f) a

pattern OnFocus :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnFocus f a = Listener (On "focus" f) a

pattern OnFocusWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnFocusWith opts f a = Listener (OnWith opts "focus" f) a

pattern OnKeyUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyUp f a = Listener (On "keyup" f) a

pattern OnKeyUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyUpWith opts f a = Listener (OnWith opts "keyup" f) a

pattern OnKeyDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyDown f a = Listener (On "keydown" f) a

pattern OnKeyDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyDownWith opts f a = Listener (OnWith opts "keydown" f) a

pattern OnKeyPress :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyPress f a = Listener (On "keypress" f) a

pattern OnKeyPressWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyPressWith opts f a = Listener (OnWith opts "keypress" f) a

pattern OnInput :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnInput f a = Listener (On "input" f) a

pattern OnInputWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnInputWith opts f a = Listener (OnWith opts "input" f) a

withInput :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withInput f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

pattern OnChange :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnChange f a = Listener (On "change" f) a

pattern OnChangeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnChangeWith opts f a = Listener (OnWith opts "change" f) a

withValue :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withValue f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj

pattern OnCheck :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnCheck f a = Listener (On "change" f) a

pattern OnCheckWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCheckWith opts f a = Listener (OnWith opts "change" f) a

withChecked :: (Bool -> IO ()) -> (Evt -> IO ())
withChecked f = traverse_ f . join . fmap (.# "checked") . (.# "target") . evtObj

-- Helpers

relatedTarget :: Evt -> Maybe JSV
relatedTarget = (.# "relatedTarget") . evtObj

pattern RelatedTarget t <- (relatedTarget -> Just t)

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

