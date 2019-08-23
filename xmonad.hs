{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


--- Inspiration https://github.com/ruhatch/.dotfiles/blob/master/.xmonad/xmonad.hs

import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare

myTerminal :: String
myTerminal = "termite"

---------------- Layout

myLayout = avoidStruts (mySpacing (Tall 1 (3 / 100) (1 / 2)))
  ||| fullscreenFull Full
 where
  mySpacing = spacingRaw False (Border 8 8 8 8) True (Border 0 0 0 0) False .
    spacingRaw True (Border 8 8 8 8) False (Border 8 8 8 8) True


-------------- Colors and borders

myNormalBorderColor :: String
myNormalBorderColor = "#FAFAFA"

myFocusedBorderColor :: String
myFocusedBorderColor = myNormalBorderColor

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#Af745f"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 0


---------------------- Keybindings


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { modMask } =
  M.fromList
    $ [

    -- Start the terminal specified by myTerminal variable
        ((modMask, xK_Return), spawn $ XMonad.terminal conf)
      ,

    -- Decrease brightness
        ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
      ,

    -- Increase brightness
        ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
      ,

    -- Move to the next empty workspace
        ((modMask, xK_e), moveTo Next EmptyWS)
      , ((modMask .|. mod1Mask, xK_w), toggleHDMI)
      ,

    -- Move focus to the next window
        ((modMask, xK_Tab), windows W.focusDown)
      ,

    -- Move focus to the next window
        ((modMask, xK_j), windows W.focusDown)
      ,

    -- Move focus to the previous window
        ((modMask, xK_k), windows W.focusUp)
      ,

    -- Move focus to the master window
        ((modMask, xK_m), windows W.focusMaster)
      ,

    -- Swap the focused window and the master window
        ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
      ,

    -- Quit xmonad
        ((modMask .|. shiftMask, xK_q), io exitSuccess)
      ,

    -- Restart xmonad
        ((modMask, xK_q), restart "xmonad" True)
      ]


toggleHDMI :: X ()
toggleHDMI = do
  (count :: Int) <- countScreens
  spawn $ "echo " ++ show count ++ " >> ~/test.txt"
  if count > 1
    then spawn "xrandr --output HDMI1 --off"
    else spawn "sleep 0.3; xrandr --output HDMI1 --auto --right-of eDP1"


------------------------- Startup

myStartupHook :: X ()
myStartupHook =
  spawn
      "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17"
    <+> setDefaultCursor xC_left_ptr
    <+> spawn "hsetroot -solid '#D6D6D6'"
    <+> spawn "xsetroot -cursor_name left_ptr"
    <+> spawn "xrandr --output HDMI1 --off"
    <+> spawn "xrandr --output HDMI1 --auto --right-of eDP1"
    <+> setWMName "LG3D"


--------------------- Loghook

myLogHook :: Handle -> X ()
myLogHook xmproc = 
  fadeInactiveLogHook 0.8 <+>
  (dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = const ""
    , ppLayout  = const ""
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep     = "   "
    })



---------------------- Run xmonad


main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . E.ewmh $ defaults { logHook = myLogHook xmproc }

--------------------- Combine

defaults = def
  {
    -- simple stuff
    terminal      = myTerminal
  , borderWidth   = myBorderWidth
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , manageHook = manageDocks <+> manageHook defaultConfig

    -- key bindings
  --, keys = myKeys

    -- hooks, layouts
  , layoutHook    = myLayout
  , startupHook   = myStartupHook
  , handleEventHook = E.fullscreenEventHook --handleEventHook defaultConfig <+> docksEventHook
  }
