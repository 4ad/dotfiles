[ -f ~/.profile ] && . ~/.profile

if [ -x "`which direnv 2>/dev/null`" ]; then
	eval "$(direnv hook bash)"
fi
