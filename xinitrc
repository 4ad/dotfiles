#!/bin/sh

[ -f ~/.profile ] && . ~/.profile

# disable bell
xset -b

[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

[ -n "$PLAN9" ] && ~/.p9setup
[ -x /usr/bin/x-www-browser ] && /usr/bin/x-www-browser &

# update the bar
while true
do
	localtime=$(date +Local\=%H:%M)
	pullman=$(TZ=US/Pacific date +Hari\=%H:%M)
	vienna=$(TZ=Europe/Vienna date +Wien\=%H:%M)
	sibiu=$(TZ=Europe/Bucharest date +Sibiu\=%H:%M)

	if [ -x /usr/bin/acpi ]
	then
		bat="Battery $(acpi -b | awk '{ print $4 " " $5 }' | tr -d ',')"
	else
		bat="Battery ???"
	fi

	xsetroot -name "$bat $localtime $pullman $vienna $sibiu"
	sleep 20s
done &

dwm
