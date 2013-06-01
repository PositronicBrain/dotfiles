export PATH=$PATH:/sbin:/usr/sbin:/home/federico/.cabal/bin
export TEXINPUTS=.:$TEXINPUTS
export USER=`id -un`
export HOSTNAME=`hostname`
export EDITOR="vi"
export BROWSER="firefox"
export SDL_AUDIODRIVER="alsa"
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
#eval `dircolors ~/.dircolors`

#bash file completion
#if [ -f /usr/share/bash-completion/bash_completion ]; then
#    .  /usr/share/bash-completion/bash_completion
#fi

# Nobody should read my files
umask 077

#prompt
PS1="\[\e[1;32m\][\u @ \h \w]\[\e[0m\]\n$ "

#Aliases

alias cp='cp -vi'
alias rm='rm -vi'
alias mv='mv -vi'

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
  dir='/usr/share/cows/'
  file=`/bin/ls -1 "$dir" | sort --random-sort | head -1`
  cow=$(echo "$file" | sed -e "s/\.cow//")
  /usr/bin/fortune -as | cowsay -f $cow
fi
