------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeys)
 
main =
    xmonad $ defaultConfig
        { handleEventHook = fullscreenEventHook
        , layoutHook = myLayout
        , manageHook = myManageHook <+> manageHook defaultConfig
        } `additionalKeys`
        ( myScrotKeys )

myManageHook = composeAll
    [ className =? "x-www-browser" --> doShift "1"
    , className =? "google-chrome" --> doShift "1"
    , className =? "chromium-browser" --> doShift "1"
    , className =? "opera" --> doShift "1"
    , className =? "firefox" --> doShift "1"
    , className =? "acme" --> doShift "2"
    , manageDocks
    ]

myLayout = smartBorders tiled ||| smartBorders mirrorTiled ||| noBorders Full
  where
    tiled = mouseResizableTile
        { draggerType = BordersDragger 
        }
    mirrorTiled = mouseResizableTile
        { isMirrored = True
        , draggerType = BordersDragger
        }

myScrotKeys = 
    [ ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s") -- take a screen shot of a window
    , ((0, xK_Print), spawn "scrot") -- take a screen shot of the screen
    ]
