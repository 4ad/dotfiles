# Zsh can't load .profile without this option.
set -o shwordsplit 
[ -f ~/.profile ] && . ~/.profile

# PS1 set in .profile doesn't work in zsh, so we fix it here.
PS1='%m:%~$ '

# ENV is set to .profile, but zsh can't load .profile directly (see
# set -o shwordsplit above) so we unset ENV. Zsh knows how to load
# this file without ENV, but other shells might suffer because ENV
# is not set. Perhaps zsh support should be removed entirely.
unset ENV
