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
export HISTIGNORE="bg:fg:ls:cd:history"
export TIMEFORMAT=$'\nreal %3R\tuser %3U\tsys %3S\tpcpu %P\n'

set -o notify
# do not overwrite files with >
set -o noclobber
# ignore CTRL-D
set -o ignoreeof

shopt -s checkwinsize
shopt -s cdspell
shopt -s no_empty_cmd_completion
shopt -s cmdhist
shopt -s histappend histreedit histverify

#colorize output
eval `dircolors ~/.dircolors`

#bash file completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    .  /usr/share/bash-completion/bash_completion
fi

# Nobody should read my files
umask 077

#prompt
PS1="\[\e[1;32m\][\u @ \h \w]\[\e[0m\]\n$ "

#Aliases

alias cp='cp -vi'
alias rm='rm -vi'
alias mv='mv -vi'
# Add colors for filetype and  human-readable sizes by default on 'ls':
alias ls='ls -h --color'
alias lx='ls -lXB'         #  Sort by extension.
alias lk='ls -lSr'         #  Sort by size, biggest last.
alias lt='ls -ltr'         #  Sort by date, most recent last.
alias lc='ls -ltcr'        #  Sort by/show change time,most recent last.
alias lu='ls -ltur'        #  Sort by/show access time,most recent last.
alias ll="ls -lv --group-directories-first"
alias lm='ll |more'        #  Pipe through 'more'
alias lr='ll -R'           #  Recursive ls.
alias la='ll -A'           #  Show hidden files.

alias back='cd $OLDPWD'

# Nice colors for manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'


#greeting message
if [[ -x /usr/bin/fortune &&  -x /usr/bin/cowsay ]]; then
    fortune -a | \
        cowsay -f $(/bin/ls /usr/share/cows -1 | \
        head -n $(expr $$$(date +%s) % $(ls /usr/share/cows | wc -w) + 1) | \
        tail -n 1)
fi
