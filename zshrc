# Zsh can't load .profile without this option.
set -o shwordsplit
# When testing, remember that this sources the file in $HOME.
[ -f ~/.profile ] && . ~/.profile

# PS1 set in .profile doesn't work in zsh, so we fix it here.
PS1='%m:%1d$ '

# Redo what we did in .profile, but unset above.
if [ "$TERM" = 9term -o "$TERM" = dumb ]; then
	# unfuck TERM=dumb
	unset zle_bracketed_paste
	unsetopt prompt_cr
	# Set prompt so we can execute whole line
	# without $PS1 interfering.
	PS1=': %m:%1d; '
fi

# ENV is set to .profile, but zsh can't load .profile directly (see
# set -o shwordsplit above) so we unset ENV. Zsh knows how to load
# this file without ENV, but older shells started from zsh might
# suffer because ENV is not set. This is unfortunate.
unset ENV
