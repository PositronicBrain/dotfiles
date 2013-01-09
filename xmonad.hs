-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Federico Squartini 2012
-- License     :  BSD-style
-- Maintainer  :  federico.squartini@gmail.com

-----------------------------------------------------------------------------
module Main where

import Control.Monad  (liftM2)
import Data.List (sort)
import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS (shiftToNext, nextWS,
                               shiftToPrev, prevWS,
                               nextScreen)
import XMonad.Actions.GridSelect (defaultGSConfig,
                                  spawnSelected)
import XMonad.Actions.Search (SearchEngine, dictionary,
                              google, hackage, isohunt,
                              maps, thesaurus,
                              wikipedia, youtube,
                              selectSearch, promptSearch)
import XMonad.Actions.WindowGo (raiseBrowser, raiseEditor, raiseMaybe)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP,
                                ppOutput,ppTitle,
                                shorten,sjanssenPP,xmobarColor)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(isFullscreen ,doFullFloat, doCenterFloat,doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Prompt (XPConfig,defaultXPConfig,font,
                      bgColor,
                      fgColor,
                      borderColor)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet as W (focusUp, focusDown, sink,shift,
                             swapUp,swapDown,shiftMaster,swapMaster,
                             view,greedyView)
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

import System.Exit (exitWith, ExitCode(..) )
import System.IO


main :: IO ()
main= do xmproc <- spawnPipe xmobarCmd
         xmonad defaultConfig {
                  modMask = myModMask,
                  -- smartborders removes borders when logical (e.g. fullscreen)
                  layoutHook =  avoidStruts $ smartBorders
                                myLayout,
                  keys = myAltKeymap,
                  terminal = myTerminal,
                  mouseBindings      = myMouseBindings,
                  startupHook = setWMName "LG3D",
                  XMonad.workspaces = ws,
                  manageHook = myManageHook <+> manageDocks <+>
                               manageHook defaultConfig,
                  logHook = dynamicLogWithPP $ sjanssenPP
                       { ppOutput = hPutStrLn xmproc,
                         ppTitle = xmobarColor "blue" "" . shorten 100
                  },
                  focusFollowsMouse = True

}

-- ^ Workspace names

ws :: [WorkspaceId]
ws = ["1:terms","2:emacs","3:web","4:files","5:chat","6:gimp"] ++
     map show [7..9::Int]

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
myManageHook = (composeAll $ concat $
    [ [resource     =? r            --> doIgnore             |   r   <- myIgnores]
    , [className    =? c            --> viewShift "1:terms"  |   c   <- terms    ]
    , [className    =? c            --> viewShift "2:emacs"  |   c   <- dev      ]
    , [className    =? c            --> viewShift "3:web"    |   c   <- web      ]
    , [className    =? c            --> viewShift "4:files"  |   c   <- files    ]
    , [className    =? c            --> viewShift "5:chat"   |   c   <- chat     ]
    , [className    =? c            --> viewShift "6:gimp"   |   c   <- gimp     ]
    , [className    =? c            --> doCenterFloat        |   c   <- floats   ]
    , [isFullscreen                 --> myDoFullFloat                            ]
    ]) <+> manageDocks

    where
        viewShift =  doF . liftM2 (.) W.greedyView W.shift

        -- classnames
        terms = ["Evilvte"]
        files = ["Thunar","Evince","Gnome-documents","Jabref"]
        floats  = ["MPlayer","XFontSel","Mplayer2","mplayer2"]
        web    = ["Firefox","Chromium"]
        dev    = ["Emacs","Gitg"]
        chat	  = ["Pidgin","Buddy List"]
        gimp	  = ["Gimp"]


        -- resources
        myIgnores = []


        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

-- ^ Where to find the xmobar binary

xmobarCmd :: String
xmobarCmd = "/usr/bin/xmobar  ~/.xmobarrc"

-- ^ Which terminal to use (evilvte)

myTerminal :: String
myTerminal = "evilvte"

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
             "emacs",
             "baobab",
             "gnome-disks",
             "gnome-documents",
             "seahorse",
             "jabref",
             "pidgin",
             "xscreensaver-demo",
             "chromium",
             "gthumb",
             "thunderbird",
             "transmission-gtk"]


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
           { font = myFont,
             bgColor = backgroundColor,
             fgColor = textColor,
             borderColor = lightBackgroundColor
}

myFont :: String
myFont = "xft:Bitstream Vera Sans:size=12"


textColor :: String
textColor = "#c0c0a0"

backgroundColor :: String
backgroundColor = "#304520"

lightBackgroundColor :: String
lightBackgroundColor  = "#456030"


-- ^ Keybindings

myAltKeymap :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myAltKeymap conf = mkKeymap conf $
    [-- close the window
     ("M-k", kill),
     -- show an application list, se the function "menu" for changing
     -- applications
     ("M-a", spawnSelected defaultGSConfig menu),
     -- Rotate forward through the available layout algorithms
     ("M-<Tab>", sendMessage NextLayout),
     -- Quit xmonad
     ("M-q M-q", io (exitWith ExitSuccess)),
     -- Restart xmonad
     ("M-Q M-Q", recompile True >> restart "xmonad" True),
     ("M-z", withFocused $ windows . W.sink), --unfloat
     -- Launch the default editor defined in the shell variable
     -- @$@EDITOR
     ("M-e", raiseEditor),
     -- Launch the default browswe defined in the shell variable
     -- @$@BROWSER
     ("M-b", raiseBrowser),
     -- launch thunderbird
     ("M-m", raiseMaybe (spawn "thunderbird")
                   (className =? "Firefox")),
     -- window navigation keybindings
     -- Move focus to the previous window
     ("M-p", windows W.focusDown),
     -- Move focus to the next window
     ("M-n", windows W.focusUp),
     -- Swap the focused window and the master window
     ("M-<Space>", windows W.swapMaster),
     -- Swap the focused window with the next window
     ("M-u", windows W.swapUp),
     -- Swap the focused window with the previous window
     ("M-d", windows W.swapDown),
     ("M-<Return>", spawn myTerminal),
     -- take a screenshot of the desktop, requires "scrot"
     ("<Print>", spawn "scrot"),
     -- lock the screen
     ("M-l", spawn "xscreensaver-command -lock"),
     -- show the shell prompt
     ("M-<F1>", shellPrompt defaultXPConfig),
     -- show ssh prompt
     ("M-<F2>", sshPrompt defaultXPConfig),
     -- show man page prompt
     ("M-<F3>", manPrompt defaultXPConfig),
     ("M-W", shiftToNext >> nextWS),
     ("M-v", shiftToPrev >> prevWS)] ++
    -- see searchList for search options
    [("M-s " ++ k, promptSearch defaultXPConfig f) |
     (k,f) <- searchList ] ++
    [("M-S-s"  ++ k, selectSearch f) | (k,f) <- searchList ] ++

    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
      | (i, k) <- zip (XMonad.workspaces conf) "123456789" -- (0)
    , (f, m) <- [ (windows . W.view, "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]

-- ^ Mouse bindings (moving and resizing windows)

myMouseBindings :: XConfig Layout ->
                   M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1 %! Set the window to floating mode
    -- and move by dragging
    [((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)),
     -- mod-button2 %! Raise the window to the top of the stack

     ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)),

     -- mod-button3 %! Set the window to floating mode
     --and resize by dragging

     ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
     -- you may also bind events to the mouse scroll wheel
     -- (button4 and button5)
    ]

-- ^ A list of search engines

searchList :: [(String, SearchEngine)]
searchList = [("d",dictionary),
              ("h",hackage),
              ("g", google),
              ("i",isohunt),
              ("t",thesaurus),
              ("m",maps),
              ("y",youtube),
              ("w", wikipedia)]
