##### GLOBAL CONFIGURATION VARIABLES #####
export PATH=$PATH:/sbin:/usr/sbin:/home/federico/.cabal/bin
export SDL_AUDIODRIVER="alsa"
export QT_STYLE_OVERRIDE="gtk"
export SHELL="/bin/bash"
export EDITOR="vim"
export BROWSER="firefox"
export LANG="en_US.UTF-8"
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp \
       -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on \
       -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

keyboardLayout=us

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
ifExistsDo "/usr/bin/nitrogen" "nitrogen --restore"

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
    --alpha 0 --height 25"

# unclutter removes the mouse after a timeout
ifExistsDo "/usr/bin/unclutter" "unclutter -root -idle 10 -jitter 10"

# keyring manager
ifExistsDo "/usr/bin/gnome-keyring-daemon" "gnome-keyring-daemon  --start"

# Policy kit authentication agent
ifExistsDo "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

# automount
ifExistsDo "/usr/bin/thunar" "thunar --daemon"

# Jack sound server
jack_control start
#sudo schedtool -R -p 20 `pidof jackdbus`
jack_control eps realtime true
jack_control ds alsa
jack_control dps device hw:0,0
#jack_control dps rate 48000
#jack_control dps nperiods 2
#jack_control dps period 64
ifExistsDo "/usr/bin/qjackctl" "qjackctl"

ifExistsDo "/usr/bin/alsa-tray" "alsa-tray"
# tracker
#ifExistsDo "/usr/bin/tracker-control" "tracker-control -s"

# dropbox
ifExistsDo "/usr/bin/dropboxd" "dropboxd"

# audio player
ifExistsDo "/usr/bin/quodlibet" "quodlibet"

# gtk-redshift
ifExistsDo "/usr/bin/redshift-gtk" "redshift-gtk"

# Virtualbox guest additions
#ifExistsDo "/usr/bin/VBoxClient-all" "VBoxClient-all"

#Window manager
exec dbus-launch --exit-with-session xmonad
