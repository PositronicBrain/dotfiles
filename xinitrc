##### GLOBAL CONFIGURATION VARIABLES #####
export PATH=$PATH:/sbin:/usr/sbin:/home/federico/.cabal/bin
export SDL_AUDIODRIVER="pulse"
export SHELL="/bin/bash"
export EDITOR="vim"
export BROWSER="firefox"
export LANG="en_US.UTF-8"
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp \
       -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on \
       -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

keyboardLayout=us
background="/usr/share/backgrounds/gnome/Terraform-green.jpg"

###### Helper functions #####

catError="osd_cat --font=\"-*-dejavu sans-*-*-*-*-30-*-*-*-*-*-*-*\" \
         -p bottom -s2 --offset=500 \
         --indent=500"

ifExistsDo () {
    if [[ -x $1 ]]; then
        $2 &
    else
        echo "$1 not found!" | eval $catError
    fi
}



###### Startup programs #####

# Enable restarting x
ifExistsDo  "/usr/bin/setxkbmap" "setxkbmap -option terminate:ctrl_alt_bksp"

# set the cursor
ifExistsDo  "/usr/bin/xsetroot" "xsetroot -cursor_name left_ptr"

# set the background
ifExistsDo "/usr/bin/xsetroot" "xsetroot -mod 16 16 -fg black -bg steelblue"

# set keyboard layout
ifExistsDo  "/usr/bin/setxkbmap" "setxkbmap -layout $keyboardLayout"

#Remove caps lock
ifExistsDo  "/usr/bin/setxkbmap" "setxkbmap -option ctrl:nocaps"

# load X resources
if [[ -f $HOME/.Xdefaults ]]; then
  xrdb -merge $HOME/.Xdefaults
fi

# load customizations to keyboard map
if [[ -f $HOME/.Xmodmap ]]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi
# start screensaver
#ifExistsDo "/usr/bin/xscreensaver" "xscreensaver -no-splash"

#Set up an icon tray
ifExistsDo "/usr/bin/trayer" \
   "trayer --edge top --align right \
    --SetDockType true --SetPartialStrut true \
    --expand true --widthtype pixel --width 260 --transparent true --tint 0x000000 \
    --alpha 0 --height 21"

# unclutter removes the mouse after a timeout
ifExistsDo "/usr/bin/unclutter" "unclutter -root -idle 10 -jitter 10"

# keyring manager
 ifExistsDo "/usr/bin/gnome-keyring-daemon" "gnome-keyring-daemon  --start"

# Policy kit authentication agent
ifExistsDo "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

# automount
ifExistsDo "/usr/bin/thunar" "thunar --daemon"

#ifExistsDo "/usr/bin/nm-applet" "nm-applet"

# Power management
#ifExistsDo  "/usr/bin/xfce4-power-manager"  "xfce4-power-manager"

# gnome bluetooth
#ifExistsDo "/usr/bin/blueman-applet" "blueman-applet"

#Pulse sound server
ifExistsDo "/usr/bin/start-pulseaudio-x11" "start-pulseaudio-x11"

# small volume icon
ifExistsDo "/usr/bin/pnmixer" "pnmixer"

# tracker
#ifExistsDo "/usr/bin/tracker-control" "tracker-control -s"

# gtk-redshift
ifExistsDo "/usr/bin/gtk-redshift" "gtk-redshift"

# dropbox
ifExistsDo "/usr/bin/dropboxd" "dropboxd"

# make copy/cut/paste work
ifExistsDo "/usr/bin/parcellite" "parcellite"

# Virtualbox guest additions
#ifExistsDo "/usr/bin/VBoxClient-all" "VBoxClient-all"

#Window manager
exec dbus-launch --exit-with-session xmonad
