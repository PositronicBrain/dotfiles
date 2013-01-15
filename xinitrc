##### CONFIGURATION VARIABLES #####
export SDL_AUDIODRIVER="alsa"
export SHELL="/bin/bash"
export EDITOR="emacs"
export BROWSER="chromium"
export LANG="en_US.UTF-8"
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp \
       -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on \
       -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

keyboardLayout=it
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


# set the cursor
ifExistsDo  "/usr/bin/xsetroot" "xsetroot -cursor_name left_ptr"

# set keyboard layout
ifExistsDo  "/usr/bin/setxkbmap" "setxkbmap -layout $keyboardLayout"

#Remove caps lock
ifExistsDo  "/usr/bin/setxkbmap" "setxkbmap -option ctrl:nocaps"

# load X resources
if [ -f $HOME/.Xdefaults ]; then
  xrdb -merge $HOME/.Xdefaults
fi

# load customizations to keyboard map
if [ -f $HOME/.Xmodmap ]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi

# start screensaver
#ifExistsDo "/usr/bin/xscreensaver" "xscreensaver -no-splash"

#Set up an icon tray
ifExistsDo "/usr/bin/trayer" \
   "trayer --edge top --align right \
    --SetDockType true --SetPartialStrut true \
    --expand true --widthtype pixel --width 224 --transparent true --tint 0x000000 \
    --alpha 0 --height 21"

# unclutter removes the mouse after a timeout
ifExistsDo "/usr/bin/unclutter" "unclutter -root -idle 10 -jitter 10"

# Set the background color
#xsetroot -solid black

ifExistsDo "/usr/bin/habak" "habak $background"

# keyring manager
ifExistsDo "/usr/bin/gnome-keyring-daemon" "gnome-keyring-daemon  --start"

# Policy kit authentication agent
ifExistsDo "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

# automount
ifExistsDo "/usr/bin/thunar" "thunar --daemon"

ifExistsDo "/usr/bin/nm-applet" "nm-applet"

# Power management
#ifExistsDo  "/usr/bin/xfce4-power-manager"  "xfce4-power-manager"

# gnome bluetooth
#ifExistsDo "/usr/bin/blueman-applet" "blueman-applet"

# small volume icon
ifExistsDo "/usr/bin/volti" "volti"

# tracker
ifExistsDo "/usr/bin/tracker-control" "tracker-control -s"

# gtk-redshift
ifExistsDo "/usr/bin/gtk-redshift" "gtk-redshift"

# dropbox
ifExistsDo "/usr/bin/dropboxd" "dropboxd"

# make copy/cut/paste work
ifExistsDo "/usr/bin/parcellite" "parcellite"


# start xmonad
exec dbus-launch /usr/bin/xmonad
