[[ -f $HOME/.rc ]] && . $HOME/.rc

# PS1 set in .rc doesn't work in zsh, so we fix it here.
PS1='%m:%1d$ '

# Redo what we did in .rc, but unset above.
if [ "$TERM" = 9term ] || [ "$TERM" = dumb ]; then
	# Disable readline and bracketed paste.
	unsetopt zle
	unset zle_bracketed_paste
	unsetopt prompt_cr
	# Set prompt so we can execute whole line
	# without $PS1 interfering.
	PS1=': %m:%1d; '
fi

if command -v direnv >/dev/null 2>&1; then
	eval "$(direnv hook zsh)"
fi
