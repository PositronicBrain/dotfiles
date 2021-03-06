-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Federico Squartini 2012
-- License     :  BSD-style
-- Maintainer  :  federico.squartini@gmail.com

-----------------------------------------------------------------------------
module Main where

import Control.Monad  (liftM2)
import Data.List (sort)

import XMonad
import XMonad.Actions.GridSelect (defaultGSConfig,
                                  spawnSelected)
import XMonad.Actions.WindowGo (raiseBrowser)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat,doCenterFloat,doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.EwmhDesktops  (fullscreenEventHook)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet as W (focusDown, sink,shift,
                             greedyView)
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.DynamicLog
import System.IO

myNormalBorderColor :: String
myNormalBorderColor  = "#111"
myFocusedBorderColor :: String
myFocusedBorderColor = "cadetblue3"


oxyXPConfig :: XPConfig
oxyXPConfig = defaultXPConfig { font              = "xft:Bitstream Vera Sans:pixelsize=14"
                              , bgColor           = "Aquamarine3"
                              , fgColor           = "black"
                              , fgHLight          = "black"
                              , bgHLight          = "darkslategray4"
                              , borderColor       = "black"
                              , promptBorderWidth = 1
                              , position          = Bottom
                              , height            = 24
                              , defaultText       = []
                              }
-- ^ Workspace names

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

-- ^ Layout order

myLayout = onWorkspace "5:chat" pidginLayout $ tiled ||| Full ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 0.55

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

--      gridLayout = Grid

     pidginLayout = withIM (18/100) (Role "buddy_list") Grid


-- ^ Program/Workspace hooks
myManageHook :: ManageHook
myManageHook = composeAll (concat
    [ [resource     =? r            --> doIgnore        |   r   <- myIgnores]
    , [className    =? c            --> viewShift "1"   |   c   <- terms    ]
    , [className    =? c            --> viewShift "2"   |   c   <- audio    ]
    , [className    =? c            --> viewShift "3"   |   c   <- web      ]
    , [className    =? c            --> viewShift "4"   |   c   <- files    ]
    , [className    =? c            --> viewShift "5"   |   c   <- chat     ]
    , [className    =? c            --> viewShift "6"   |   c   <- gimp     ]
    , [className    =? c            --> viewShift "7"   |   c   <- inkscape ]
    , [className    =? c            --> viewShift "8"   |   c   <- blender  ]
    , [className    =? c            --> viewShift "9"   |   c   <- vbox     ]
    , [className    =? c            --> doCenterFloat   |   c   <- cfloats  ]
    , [className    =? c            --> doFloat         |   c   <- floats   ]
    , [isFullscreen                 --> myDoFullFloat                              ]
    ]) <+> manageDocks

    where
        viewShift =  doF . liftM2 (.) W.greedyView W.shift

        -- classnames
        terms    = ["Evilvte", "termite", "Termite"]
        files    = ["Thunar","Jabref","pcmanfm","Pcmanfm"]
        cfloats  = ["mpv","XFontSel","mpv","mpv",
                    "Default - Wine desktop","Wine","openttd"]
        floats   = ["Mini (bristol)","Bristol gui","BasicWin","Mini",
                    "Qsynth","Vmpk","Yoshimi","Qjackctl",
                    "Hydrogen","Amsynth","Aeolus"]
        web      = ["Firefox","Chromium","chromium"]
        chat     = ["Pidgin","Buddy List"]
        gimp     = ["Gimp"]
        inkscape = ["Inkscape"]
        blender  = ["Blender"]
        vbox     = ["VirtualBox"]
        audio    = ["Amsynth","Aeolus","Qjackctl","vmpk",
                    "Hydrogen","BasicWin","Qsynth","Vmpk","Yoshimi"]
        --
        -- resources
        myIgnores = []


        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

-- ^ Where to find the xmobar binary

xmobarCmd :: String
xmobarCmd = "/usr/bin/xmobar  ~/.xmobarrc"

-- ^ Which terminal to use (termite)

myTerminal :: String
myTerminal = "termite"

-- ^ Key for combinations (Win)

myModMask :: KeyMask
myModMask = mod4Mask -- Win key

-- ^ List of program names for the graphical menu

menu :: [String]
menu = sort ["arandr",
             "lxappearance",
             "cheese",
             "evince",
             "quodlibet",
             "thunar",
             "pcmanfm",
             -- "vmpk",
             -- "yoshimi",
             -- "amsynth",
             -- "aeolus",
             -- "qsynth",
             -- "startBristol -mini -quality 8 -scale 2 -preload 8 -nnp -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -odyssey -quality 8 -scale 2 -preload 8 -nnp -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -arp2600 -quality 6 -scale 2 -preload 8 -nnp -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -dx -quality 8 -scale 2 -preload 8 -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -solina -quality 8 -scale 2 -preload 8 -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -juno -quality 8 -scale 2 -preload 8  -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -prophet -quality 8 -scale 2 -preload 8  -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -obx -quality 8 -scale 2 -preload 8  -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "startBristol -b3 -quality 6 -scale 2 -preload 8 -wwf -rate 48000  -blofraction 0.5 -blo 64",
             -- "hydrogen",
             "baobab",
             "gnome-disks",
             "paprefs",
             "pavucontrol",
             "gnome-documents",
             "seahorse",
             "jabref",
             "gpick",
             "pidgin",
             "xscreensaver-demo",
             "firefox",
             "geeqie",
             "inkscape",
             "blender",
             "gimp",
             "font-manager",
             "thunderbird",
             "virtualbox",
             "transmission-gtk"]

myFont :: String
myFont = "xft:Bitstream Vera Sans:size=12"


textColor :: String
textColor = "#c0c0a0"

backgroundColor :: String
backgroundColor = "#304520"

lightBackgroundColor :: String
lightBackgroundColor  = "#456030"

oxyPP :: Handle -> PP
oxyPP h = defaultPP  { ppCurrent = wrap "<fc=black,aquamarine3> " " </fc>"
                     , ppSep     = ""
                     , ppWsSep = ""
                     , ppVisible = wrap "<fc=black,DarkSlateGray4> " " </fc>"
                     , ppLayout = \x -> "<fc=aquamarine2,black>:: " ++
                                        case x of
                                          "Mirror ResizableTall"   -> "MTiled"
                                          "ResizableTall"          -> "Tiled"
                                          "Tabbed Bottom Simplest" -> "Tabbed"
                                          "Tabbed Simplest"        -> "Tabbed"
                                          _                        -> x
                                          ++ "</fc> "
                     , ppTitle = \x -> case length x of
                                           0 -> ""
                                           _ -> "<fc=DarkSlateGray3,black>[" ++ shorten 60 x ++ "]</fc>"
                     , ppHiddenNoWindows = const ""
                     , ppHidden = wrap "<fc=#aaa,black> " " </fc>"
                     , ppOutput = hPutStrLn h
                     }
-- ^ Additional keybindings

myKeymap :: [(String, X ())]
myKeymap =
    [ -- show an application list, se the function "menu" for changing
     -- applications
     ("M-a", spawnSelected defaultGSConfig menu),
     ("M-z", withFocused $ windows . W.sink), --unfloat
     -- Launch the default browswe defined in the shell variable
     -- @$@BROWSER
     ("M-b", raiseBrowser),
     -- take a screenshot of the desktop, requires "scrot"
     ("<Print>", spawn "scrot"),
     -- show the shell prompt
     ("M-<F1>", shellPrompt oxyXPConfig),
     -- show ssh prompt
     ("M-<F2>", sshPrompt oxyXPConfig),
      -- show man page prompt
     ("M-<F3>", manPrompt oxyXPConfig)
    ]

trayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width 260 --transparent true --tint 0x000000 --alpha 0 --height 25"

main :: IO ()
main= do xmobarPipe <- spawnPipe xmobarCmd
         spawn trayer
         xmonad $ docks $ defaultConfig {
                  modMask = myModMask,
                  -- smartborders removes borders when logical (e.g. fullscreen)
                  layoutHook =  avoidStruts $ smartBorders
                                myLayout,

     handleEventHook    = fullscreenEventHook, -- fix chrome fullscreen
                  normalBorderColor = myNormalBorderColor,
                  focusedBorderColor = myFocusedBorderColor,
                  terminal = myTerminal,
                  startupHook = setWMName "LG3D",
                  XMonad.workspaces = myWorkspaces,
                --  manageHook = manageDocks <+> manageHook defaultConfig,
                  manageHook = myManageHook <+> manageHook defaultConfig,
                  logHook = dynamicLogWithPP $ oxyPP xmobarPipe,
                  focusFollowsMouse = False
                 } `additionalKeysP`    myKeymap

