function prompt_char {
	if [ $UID -eq 0 ]; then echo "#"; else echo $; fi
}

NEWLINE=$'\n'
PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m %{$fg_bold[blue]%}%(!.%1~.%~) %{$fg_bold[yellow]%}$(git_prompt_info)%_${NEWLINE}$(prompt_char)%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
