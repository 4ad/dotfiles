[ -f ~/.profile ] && . ~/.profile

# PS1 set in .profile doesn't work in zsh, so we fix it here.
PS1='%m:%~$ '
