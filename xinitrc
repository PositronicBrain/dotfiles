##### CONFIGURATION VARIABLES #####

export DE="xfce"
#export SDL_AUDIODRIVER="alsa"
export SHELL="/bin/bash"
export EDITOR="emacs"
export BROWSER="chromium"
xport LANG=en_US.UTF-8
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

keyboardLayout=it

###### Startup programs #####

catError="osd_cat --font=\"-*-dejavu sans-*-*-*-*-30-*-*-*-*-*-*-*\" \
         -p bottom -s2 --offset=500 \
         --indent=500"

#Remove caps lock
setxkbmap -option ctrl:nocaps

# set the cursor
if [ -f /usr/bin/xsetroot ]; then
    xsetroot -cursor_name left_ptr
else
    echo 'xsetroot not found!' | eval $catError
fi

# set keyboard layout
if [ -f /usr/bin/setxkbmap ]; then
    setxkbmap -layout $keyboardLayout
else
    echo 'setxkbmap not found!' | eval $catError
fi

# load X resources
if [ -f $HOME/.Xdefaults ]; then
  xrdb -merge $HOME/.Xdefaults
fi

# load customizations to keyboard map
if [ -f $HOME/.Xmodmap ]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi

# start screensaver
if [ -x /usr/bin/xscreensaver ]; then
    xscreensaver -no-splash &
fi

# Set up an icon tray
if [ -x /usr/bin/trayer ]; then
   trayer --edge top --align right --SetDockType true --SetPartialStrut true \
   --expand true --widthtype pixel --width 224 --transparent false --tint 10 \
   --height 20 &
else
    echo 'trayer not found!' | eval $catError
fi

# unclutter removes the mouse after a timeout
if [ -x /usr/bin/unclutter ]; then
    unclutter -root -idle 10 -jitter 10 &
fi

# Set the background color
#xsetroot -solid black
if [ -x /usr/bin/habak ]; then
    habak /usr/share/backgrounds/gnome/Road.jpg &
else
    echo 'haback not found!' | eval $catError
fi

# keyring manager
if [ -x /usr/bin/gnome-keyring-daemon ]; then
    gnome-keyring-daemon  --start
else
    echo 'gnome-keyring-daemon not found!' | eval $catError
fi


# Policy kit authentication agent
if [ -x /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 ]; then
    /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
else
    echo 'polkit-gnome-authentication-agent-1 not found!' | eval $catError
fi

# automount
if [ -x /usr/bin/thunar ]; then
thunar --daemon &
else
    echo 'thunar not found!' | eval $catError
fi

if [ -x /usr/bin/nm-applet ]; then
nm-applet &
else
    echo 'nm-applet not found!' | eval $catError
fi

# Power management
if [ -x /usr/bin/xfce4-power-manager ]; then
  xfce4-power-manager &
fi

# gnome bluetooth
if [ -x /usr/bin/blueman-applet ]; then
blueman-applet &
fi

# small volume icon
if [ -x /usr/bin/volumeicon ]; then
volumeicon &
fi

# tracker
if [ -x /usr/bin/tracker-control ]; then
tracker-control -s &
else
    echo 'tracker not found!' | eval $catError
fi


# make copy/cut/paste work
if [ -x /usr/bin/autocutsel ]; then
autocutsel -fork
autocutsel -selection PRIMARY -fork
fi

# start xmonad
exec /usr/bin/xmonad
