# Path to your oh-my-zsh installation.
export PATH=$HOME/dev/go/bin:$HOME/bin:/usr/local/bin:$PATH
export GOPATH=$HOME/dev/go
export ZSH=$HOME/.oh-my-zsh
export EDITOR=vim
export PAGER=vimpager

# export TERM=rxvt-256color
# export TERM=rxvt-unicode-256color

ZSH_THEME="frisk"

setopt autopushd
plugins=(git git-flow)
source $ZSH/oh-my-zsh.sh

# Aliases

# Git
alias gl='git log --stat'
alias glp='git log -p'
alias gls='git log --color --decorate --oneline'
alias glo='git log --color --decorate --graph'

alias gs='git status'

alias gaa='git add -u' # add all tracked files


# Common
alias tmns="tmux new-session -s"
alias tmat="tmux attach -t"
alias tmls="tmux list-sessions"
alias tardir="tar -zcvf"
alias ll='ls -la --color'

alias -g L="| less"
alias -g EL='|& less'
alias -g LL="2>&1 | less"

alias -g GR="| grep -n"

# Copy the working dir to the clipboard
alias cpwd='pwd|xargs echo -n|pbcopy'

alias sha1='openssl dgst -sha1'
alias sha256='openssl dgst -sha256'

alias grep='grep --color=auto'

function mdcd() {
    mkdir -p "$*" && cd "$*"
}


if ! [ $(pgrep gpg-agent) ]; then
    eval $(gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info")
fi

if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
fi

alias e='emacscli'

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

function waitpid() {
    while ps -p $1; do sleep 10 && echo "waiting for $1 ..."; done ;
}

if [ $(tput colors) -eq "256" ]; then
    source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
fi

eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install )"
alias v='f -e vim'
alias c='fasd_cd -d'
alias o='a -e xdg-open'


# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=/home/asavonic/dev/games/teapod/src/external/cocos2d/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=/home/asavonic/dev/games/teapod/src/external/cocos2d/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

# Add environment variable ANT_ROOT for cocos2d-x
export ANT_ROOT=/usr/bin
export PATH=$ANT_ROOT:$PATH
