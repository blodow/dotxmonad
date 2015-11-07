import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Named

import XMonad.Util.Themes

myTheme = donaldTheme { theme = (theme donaldTheme) { activeColor = "#D75F5F"
                                                    , activeBorderColor = "#D75F5F"
                                                    }
                      }

genericLayout =	nameTail $ maximize $ smartBorders $
      named "T" tiled
  ||| named "M" (tabbed shrinkText (theme myTheme))
  ||| named "F" simplestFloat
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1/2
  -- Percent of screen to increment by when resizing panes
  delta   = 3/100

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn h
    , ppTitle = xmobarColor "green" "" . shorten 50
    }

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/blodow/.xmonad/xmobarrc"
xmonad $ defaultConfig
  { manageHook = manageDocks <+> manageHook defaultConfig
  , normalBorderColor = "black"
  , focusedBorderColor = "#D75F5F"
  , borderWidth = 4
  , workspaces = myWorkspaces
  , logHook = logHook gnomeConfig <+> myLogHook xmproc
  --, layoutHook = avoidStruts $ spacing 2 $ layoutHook defaultConfig
  , layoutHook = avoidStruts . smartSpacing 2 $ genericLayout
  } `additionalKeys`
  [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")  --mod4mask is the windows key
  , ((0, xK_Print), spawn "gnome-screenshot")
  -- Move focus to the master window
  , ((mod4Mask, xK_m), withFocused $ sendMessage . maximizeRestore )
  ]

