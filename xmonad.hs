import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W

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

import Codec.Binary.UTF8.String
import qualified Data.Map as M
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Signals (signalProcess, sigUSR1)
import System.Posix.Types (ProcessID)

myHighlight :: String
myHighlight = "#D75F5F"

myTheme = donaldTheme { theme = (theme donaldTheme) { activeColor = myHighlight
                                                    , activeBorderColor = myHighlight
                                                    }
                      }

genericLayout =	nameTail $ maximize $ smartBorders $
      named "<icon=/home/blodow/.xmonad/icons/tiledM.xbm/>" (mySpacing tiled)
  ||| named "<icon=/home/blodow/.xmonad/icons/grid.xbm/>" (mySpacing Grid)
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
  -- space around windows unless they are the sole window on the current workspace
  mySpacing = smartSpacing 2

-- (Const False)
myLayout = onWorkspace "1" (named "IM" $ combineTwoP
                            (TwoPane 0.03 0.8) (tabbed shrinkText (theme myTheme)) Grid (ClassName "Firefox"))
           genericLayout

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn h
    , ppTitle = xmobarColor "green" "" . shorten 50
    }

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

mySpawnPipe :: MonadIO m => String -> m (Handle, ProcessID)
mySpawnPipe x = io $ do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  pid <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile "/bin/sh" False ["-c", encodeString x] Nothing
  closeFd rd
  return (h, pid)

myKeys pid conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ -- launch some apps
      ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((0, xK_Print), spawn "gnome-screenshot")
    , ((modm, xK_F1), spawn "firefox")
    , ((modm, xK_F2), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_BackSpace), io $ signalProcess sigUSR1 pid)
    -- Move focus to the master window
    , ((modm, xK_m), withFocused $ sendMessage . maximizeRestore)
    -- close focused window
    , ((modm .|. shiftMask,  xK_c), kill)
     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    -- , ((modm .|. shiftMask,  xK_space), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm, xK_r), refresh)
    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the next screen
    , ((modm, xK_BackSpace), nextScreen)
    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
    , ((modm, xK_m), withFocused $ sendMessage . maximizeRestore)
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask,  xK_Return), windows W.swapMaster)
    -- Swap window
    , ((modm, xK_o), sendMessage SwapWindow)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask,  xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask,  xK_k), windows W.swapUp)
    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap
    , ((modm, xK_b), sendMessage ToggleStruts)
    -- Quit xmonad (Default)
    , ((modm .|. shiftMask,  xK_q), io exitSuccess)
    -- Restart xmonad
    , ((modm .|. shiftMask,  xK_r),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

main = do
  (xmproc, pid) <- mySpawnPipe "/usr/bin/xmobar /home/blodow/.xmonad/xmobarrc"
  xmonad $ gnomeConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , normalBorderColor = "black"
    , focusedBorderColor = myHighlight
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 4
    , keys = myKeys pid
    , modMask = mod1Mask --mod4mask is the alt key
    , workspaces = myWorkspaces
    , logHook = logHook gnomeConfig <+> myLogHook xmproc
    , layoutHook = avoidStruts myLayout
    }

