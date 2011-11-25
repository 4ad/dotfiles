#!/bin/sh

[ -f ~/.profile ] && . ~/.profile

# disable bell
xset -b

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

[ -n "$PLAN9" ] && ~/.p9setup
[ -n "$PLAN9" ] && plumber
[ -x /usr/bin/x-www-browser ] && /usr/bin/x-www-browser &

[ -n "`which xmonad`" ] && xmonad || xterm
