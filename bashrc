export PATH=$PATH:/home/federico/.cabal/bin
export TEXINPUTS=.:$TEXINPUTS
export USER=`id -un`
export HOSTNAME=`hostname`
export EDITOR="emacs"
export BROWSER="chromium"
export SDL_AUDIODRIVER="alsa"
export JAVA_HOME="/opt/java/jre"
export PAGER='less'
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp \
                      -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on \
                      -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
export HISTSIZE=1000
export HISTCONTROL=ignoredups

#checks the window size after each command and, if necessary, updates the
#values of LINES and COLUMNS.
shopt -s checkwinsize

#colorize output
eval `dircolors ~/.dir_colors`

#bash file completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    .  /usr/share/bash-completion/bash_completion
fi

# Nobody should read my files
umask 077

#prompt
export PS1="\[\e[1;32m\][\u @ \h \w]\[\e[0m\]\n$ "

#Aliases

alias cp='cp -vi'
alias rm='rm -vi'
alias mv='mv -vi'
alias ls='ls --color=auto  -s -H -C -h -T 0 -F --group-directories-first'
alias back='cd $OLDPWD'


#greeting message
if [[ -x /usr/bin/fortune &&  -x /usr/bin/cowsay ]]; then
    fortune -a | \
        cowsay -f $(/bin/ls /usr/share/cows -1 | \
        head -n $(expr $$$(date +%s) % $(ls /usr/share/cows | wc -w) + 1) | \
        tail -n 1)
fi

#alternative greeting message
#screenfetch
