import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import XMonad.Layout.Maximize
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.ComboP
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Named

import XMonad.Util.Themes

myHighlight :: String
myHighlight = "#D75F5F"

myTheme = donaldTheme { theme = (theme donaldTheme) { activeColor = myHighlight
                                                    , activeBorderColor = myHighlight
                                                    }
                      }

genericLayout =	nameTail $ maximize $ smartBorders $
      named "<icon=/home/blodow/.xmonad/icons/tiledM.xbm/>" tiled
  ||| named "<icon=/home/blodow/.xmonad/icons/grid.xbm/>" Grid
  ||| named "<icon=/home/blodow/.xmonad/icons/tabbed.xbm/>" (tabbed shrinkText (theme myTheme))
  ||| named "<icon=/home/blodow/.xmonad/icons/float.xbm/>" simplestFloat
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1/2
  -- Percent of screen to increment by when resizing panes
  delta   = 3/100

-- (Const False)
myLayout = onWorkspace "1" (named "IM" $ combineTwoP
                            (TwoPane 0.03 0.8) (tabbed shrinkText (theme myTheme)) Grid (ClassName "Firefox")) $
           genericLayout

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn h
    , ppTitle = xmobarColor "green" "" . shorten 50
    }

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

modm = mod4Mask  --mod4mask is the windows key

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/blodow/.xmonad/xmobarrc"
xmonad $ defaultConfig
  { manageHook = manageDocks <+> manageHook defaultConfig
  , normalBorderColor = "black"
  , focusedBorderColor = myHighlight
  , borderWidth = 4
  , workspaces = myWorkspaces
  , logHook = logHook gnomeConfig <+> myLogHook xmproc
  --, layoutHook = avoidStruts $ spacing 2 $ layoutHook defaultConfig
  , layoutHook = avoidStruts . smartSpacing 2 $ myLayout
  } `additionalKeys`
  [ ((modm .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")
  , ((0, xK_Print), spawn "gnome-screenshot")
  -- Move focus to the master window
  , ((modm, xK_m), withFocused $ sendMessage . maximizeRestore )
  -- Swap window
  , ((modm, xK_o), sendMessage $ SwapWindow)
  ]

