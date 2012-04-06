-- ~/.xmonad/xmonad.hs
--
-- XMonad config file
--
-- Known flaws:
--
--    - spawns an instance of dzen and conky on <Meta + q>
--    - can't split the status bar based on the screen width
--
-- ## Imports ##
import XMonad
import XMonad.StackSet as W

import XMonad.Config.Gnome

import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)

import Control.Monad (guard)
import XMonad.Operations
 
import System.IO
import System.Exit
import System.Environment (getArgs)

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
 
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
 
import Data.Ratio ((%))
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- ## Init ##

modMask' :: KeyMask
modMask' = mod4Mask

-- commands
xTerminal = "gnome-terminal"
xWorkspaces = ["1:main","2:web","3:email","4:chat","5:vim"]
xMonadBar  = "dzen2 -x '0' -y '0' -h '24' -w '866' " ++
             "-ta 'l' -fg '#FFFFFF' -bg '#1B1D1E' " ++
             "-fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*'"
xStatusBar  = "conky -c ~/.xmonad/conky_dzen.conf | " ++
              "dzen2 -x '866' -y '0' -w '500' -h '24' " ++
              "-ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' " ++
              "-fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*'"
xBitmapsDir = "/home/ecto/.xmonad/dzen_icons"
xScreensaver = "gnome-screensaver"
xBrowser = "firefox"
xMUA = "/opt/thunderbird/thunderbird"
xEditor = "gvim"
xRun = "bashrun"

-- colors
xNormalBorderColor   = "#CCCCC6"
xFocusedBorderColor  = "#fd971f"

-- ## Main configuration ##
main :: IO ()
main = do
   dzenLeftBar <- spawnPipe xMonadBar
   xmonad $ defaultConfig
      { terminal            = xTerminal 
      , XMonad.workspaces   = xWorkspaces
      , modMask             = modMask'
      , startupHook         = do
                              fmap (not . null) (io getArgs)
                              spawn xStatusBar
                              --spawn xScreensaver
                              --spawn xTerminal
                              --spawn xBrowser
                              --spawn xMUA
                              --spawn xEditor

      , layoutHook          = smartBorders $ layoutHook gnomeConfig
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = xNormalBorderColor
      , focusedBorderColor  = xFocusedBorderColor
      , borderWidth         = 1
      }
      `additionalKeys`
            [ ((0,                              xK_Print), spawn "scrot -e 'mv $f ~/screenshots/'")
            , ((modMask',                       xK_Left),  withFocused $ snapMove L Nothing)
            , ((modMask',                       xK_Right), withFocused $ snapMove R Nothing)
            , ((modMask',                       xK_Up),    withFocused $ snapMove U Nothing)
            , ((modMask',                       xK_Down),  withFocused $ snapMove D Nothing)
            , ((modMask' .|. shiftMask,         xK_Left),  withFocused $ snapShrink R Nothing)
            , ((modMask' .|. shiftMask,         xK_Right), withFocused $ snapGrow R Nothing)
            , ((modMask' .|. shiftMask,         xK_Up),    withFocused $ snapShrink D Nothing)
            , ((modMask' .|. shiftMask,         xK_Down),  withFocused $ snapGrow D Nothing)
            , ((modMask',                       xK_t),     sendMessage ToggleStruts)
            , ((modMask',                       xK_f),     sendMessage ToggleStruts >> sendMessage ToggleLayout)
            , ((modMask' .|. shiftMask,         xK_s),     withFocused $ windows . W.sink)
            , ((modMask' .|. shiftMask,         xK_l),     spawn "gnome-screensaver-command -l")
            , ((modMask' .|. shiftMask,         xK_q),     spawn "gnome-session-save --logout")
            , ((modMask',                       xK_p),     spawn xRun)
            ]

-- ## Manage hooks - Shift windows to workspaces, set default layouts ##
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
   [ [resource       =? r     --> doCenterFloat       | r      <- xSpecial]
   , [className      =? c     --> doShift "1:main"    | c      <- xDev]      -- Terminals & stuff
   , [className      =? c     --> doShift "2:web"     | c      <- xWeb]      -- Browser, etc.
   , [className      =? c     --> doShift "3:email"   | c      <- xEmail]    -- Email
   , [className      =? c     --> doShift "4:chat"    | c      <- xChat]     -- Chat programs
   , [className      =? c     --> doShift "5:vim"     | c      <- xVim]      -- Dev. programs
   , [className      =? c     --> doCenterFloat       | c      <- xFloats]   -- VMs, Gimp, etc.
   , [isFullscreen            --> doFullFloat']
   ])

   where

   -- class names
   xFloats    = ["qemu-system-x86_64","Gimp"]
   xDev       = ["gnome-terminal"]
   xWeb       = ["Firefox", "Google-chrome", "Chromium"]
   xEmail     = ["Thunderbird"]
   xChat      = ["Pidgin", "Buddy List", "Skype"]
   xVim       = ["Gvim"]
   xSpecial   = ["bashrun"]

-- full float "hack"
doFullFloat' :: ManageHook
doFullFloat' = doF W.focusDown <+> doFullFloat

-- ## Log hook - Send information do dzen2, set neat little icons ##
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP 
   { ppCurrent          =   dzenColor "#ebac54" "#1B1D1E"   . pad
   , ppVisible          =   dzenColor "white" "#1B1D1E"     . pad
   , ppHidden           =   dzenColor "white" "#1B1D1E"     . pad
   , ppHiddenNoWindows  =   dzenColor "#7b7b7b" "#1B1D1E"   . pad
   , ppUrgent           =   dzenColor "#ff0000" "#1B1D1E"   . pad
   , ppWsSep            =   " "
   , ppSep              =   " | " 
   , ppLayout           =   dzenColor "#ebac54" "#1B1D1E" .
                 (\x -> case x of 
                  "Tall"                  -> "^i(" ++ xBitmapsDir ++ "/tall.xbm)"
                  "ResizableTall"         -> "^i(" ++ xBitmapsDir ++ "/tall.xbm)"
                  "Mirror Tall"           -> "^i(" ++ xBitmapsDir ++ "/mtall.xbm)"
                  "Mirror ResizableTall"  -> "^i(" ++ xBitmapsDir ++ "/mtall.xbm)"
                  "Full"                  -> "^i(" ++ xBitmapsDir ++ "/full.xbm)"
                  "Simple Float"          -> "~"
                  _ -> x
                 )
      , ppTitle            =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput           =   hPutStrLn h
   }
