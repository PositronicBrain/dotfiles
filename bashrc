export EDITOR=vim
export VISUAL=vim
export TEXINPUTS=.:$TEXINPUTS
export USER=`id -un`
export HISTSIZE=10000
export HISTCONTROL=ignoredups
export HISTIGNORE="bg:fg:ls:cd:history"
export TIMEFORMAT=$'\nreal %3R\tuser %3U\tsys %3S\tpcpu %P\n'
set -o notify
# do not overwrite files with >
set -o noclobber
# ignore CTRL-D
set -o ignoreeof
# use vi keybindings
set -o vi

shopt -s checkwinsize
shopt -s cdspell
shopt -s no_empty_cmd_completion
shopt -s cmdhist
shopt -s histappend histreedit histverify

#colorize output
eval `dircolors ~/.dir_colors`

# Nobody should read my files
umask 057

#bash file completion
if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source  /usr/share/bash-completion/bash_completion
fi

# Add a function to show git current branch in the prompt
if [[ -f /usr/share/git/completion/git-prompt.sh ]]; then
    source  /usr/share/git/completion/git-prompt.sh
fi


EXT_COLOR="\[\033[38;5;243m\]"
NO_COLOR="\[\033[0m\]"
export PS1="${EXT_COLOR}\u@\h \w$(__git_ps1 " (%s)")\n\$$NO_COLOR "

#Aliases

alias e=vim
alias cp='cp -vi'
alias rm='rm -vi'
alias mv='mv -vi'
# colorize output of ls with dircolor
alias ls='ls --color=auto'

# Man colors
export MANROFFOPT='-c'
export LESS_TERMCAP_mb=$(tput bold; tput setaf 007)
export LESS_TERMCAP_md=$(tput bold; tput setaf 007)
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 236; tput setab 007)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 240)
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
#

#greeting message
if [[ -x /usr/bin/fortune &&  -x /usr/bin/cowsay ]]; then
  /usr/bin/fortune | cowsay
fi

export NVM_DIR="/home/federico/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
