# Path to your oh-my-zsh installation.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=$HOME/.oh-my-zsh
export EDITOR=vim
export PAGER=vimpager

# export TERM=rxvt-256color
# export TERM=rxvt-unicode-256color

ZSH_THEME="frisk"
plugins=(vi-mode)

# Aliases
alias tmns="tmux new-session -s"
alias tmat="tmux attach -t"
alias tmls="tmux list-sessions"
alias tardir="tar -zcvf"
alias ll='ls -la --color'

setopt autopushd

source $ZSH/oh-my-zsh.sh

if ! [ $(pgrep gpg-agent) ]; then
    eval $(gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info")
fi

if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
fi


TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

function waitpid() {
    while ps -p $1; do sleep 10 && echo "waiting for $1 ..."; done ;
}

if [ $(tput colors) -eq "256" ]; then
    source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
fi
