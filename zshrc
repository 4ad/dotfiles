[[ -f $HOME/.shrc ]] && . $HOME/.shrc

# allow comments even in interactive shells.
setopt interactivecomments

# PS1 set in .shrc doesn't work in zsh, so we fix it here.
PS1='%m:%1d$ '

# Redo what we did in .shrc, but unset above.
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
	# Child shells inherit direnv's bookkeeping from a parent shell.
	# Clear it so each new shell reinitializes its direnv environment.
	unset DIRENV_DIFF DIRENV_DIR DIRENV_FILE DIRENV_WATCHES
	eval "$(direnv hook zsh)"
fi
