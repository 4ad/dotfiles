------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
main =
    xmonad $ defaultConfig
        { handleEventHook    = fullscreenEventHook
        } `additionalKeys`
        ( myGSKeys ++ myScrotKeys )
 
myGSConfig = defaultGSConfig

myGSKeys = 
    [ ((mod1Mask, xK_Tab), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((mod4Mask, xK_Tab), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspace
    ]

myScrotKeys = 
    [ ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s") -- take a screen shot of the window
    , ((0, xK_Print), spawn "scrot") -- take a screen shot of the screen
    ]
