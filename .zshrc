# Path to your oh-my-zsh installation.
source ~/.bashrc.asavonic
export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=$HOME/.oh-my-zsh
export EDITOR=vim
export PAGER=vimpager
export TERM=screen-256color

ZSH_THEME="frisk"

# Aliases
alias tmns="tmux new-session -s"
alias tmat="tmux attach -t"
alias tmls="tmux list-sessions"
alias tardir="tar -zcvf"
alias ll='ls -la --color'

setopt autopushd

source $ZSH/oh-my-zsh.sh
