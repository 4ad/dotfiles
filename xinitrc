#!/bin/sh

[ -f ~/.profile ] && . ~/.profile

# disable bell
xset -b

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap
[ -n `which setxkbmap` ] && setxkbmap ro >/dev/null 2>&1

[ -n "$PLAN9" ] && ~/.p9setup
[ -n "$PLAN9" ] && plumber

[ -x /usr/bin/x-www-browser ] && /usr/bin/x-www-browser &
[ -n "$PLAN9" ] && acme &

~/.startxmonad
