OS="`uname | tr A-Z a-z | sed 's/mingw/windows/; s/.*windows.*/windows/'`"

ARCH="`uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'`"
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
[ "$OS" = "darwin" ] && ARCH="`if sysctl machdep.cpu.extfeatures 2>&1 | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi`"

# Some Linux distros don't have hostname, amazing.
[ -x /bin/hostname ] && H="`hostname`" || H=$OS

export OS ARCH H

# Make sure all directories in $PATH exist,
# some tools complain if they don't.
mkdir -p ~/bin/$OS/$ARCH
BIN=.:~/bin:~/bin/$OS:~/bin/$OS/$ARCH

# If we're on amd64 and we're not on openbsd, we can
# also run 32 bit binaries.
if [ "$ARCH" = "amd64" -a "$OS" != "openbsd" ]; then
	mkdir -p ~/bin/$OS/386
	BIN=$BIN:~/bin/$OS/386
fi

# Check for Go.
[ -f ~/go/include/u.h ] && BIN=$BIN:~/go/bin

# If /bin is a symlink (some UNICES), don't add it to $PATH
[ ! -h /bin ] && BIN=$BIN:/bin
# Sorted by preference
paths="
	${HOME}/.cabal/bin
	/sbin
	/usr/bin
	/usr/sbin
	/usr/games
	/usr/local/bin
	/usr/local/sbin
	/usr/pkg/bin
	/usr/pkg/sbin
	/opt/bin
	/opt/sbin
	/opt/local/bin
	/opt/local/sbin
"
# Add to $PATH if directory exists.
for i in $paths; do
	[ -d $i ] && BIN=$BIN:$i
done

# It's safe to set $PATH here.
PATH=$BIN

# Check for Plan9 tools.
if [ -f ~/plan9/include/u.h ]; then
	PLAN9=~/plan9
	PATH=$PATH:$PLAN9/bin

	mkdir -p ~/lib

	font="$PLAN9/font/luc/unicode.7.font"

	alias acme="acme -a -f $font -F $PLAN9/font/fixed/unicode.8x13.font -l ~/lib/acme.dump"
	alias sam='sam -a'
	
	# Plumb files instead of starting new editor.
	# Only if running X.
	if [ -n "$DISPLAY" ]; then
		EDITOR=E
		unset FCEDIT VISUAL
	fi

	# Let gs find the plan9port document fonts.
	GS_FONTPATH=$PLAN9/postscript/font

	# Make man work in 9term and acme's win,
	# but use a traditional pager under UNIX
	_man() {
		if [ "$TERM" = "9term" ]; then
			PAGER=nobs man "$@"
		else
			man "$@"
		fi
	}
	alias man=_man

	# Equivalent variables for rc(1).
	home=$HOME
	user=$USER
	prompt="$H=;"

	# Keep the label up to date, so plumber works,
	# but only if X is running.
	if [ -n "$DISPLAY" ]; then
		_cd () {
			\cd "$@" &&
			case $- in
			*i*)
				awd
			esac
		}
		alias cd=_cd
		cd .
	fi

	# If running in 9term or acme, make the environment
	# more Plan9 like.
	if [ "$TERM" = "9term" ]; then
		# Disable readline
		set +o emacs
		set +o vi
		# Enable autoexport, like in rc(1).
		set -a
	fi

	export PLAN9 font EDITOR FCEDIT VISUAL GS_FONTPATH home user prompt 
fi

# Browsers, in order of preference.
browsers="
	firefox
	opera
	chromium-browser
	google-chrome
	x-www-browser
"
# Try to set BROWSER (used by the plumber)
for i in $browsers; do
	if which $i >/dev/null 2>&1; then
		export BROWSER="$i"
	fi
done

# A simple prompt
PS1='$(printf "%s" "${H}:${PWD/${HOME}/~}\$ ")'
# Above prompt doesn't work in zsh, fixed in .zshrc

# Some aliases.
alias ls='ls -F'
alias ll='ls -l'
alias la='ls -lA'

# Some shells source $ENV when they're interactive
export ENV=~/.profile

# Try to start X if it isn't started yet and we logged in
# on tty1
if [ "`/bin/ls -l /proc/self/fd/0 2>/dev/null | awk '{print $NF}'`" = '/dev/tty1' ];
then
	[ -z "$DISPLAY" ] && [ -n "`which startx`" ] && startx
fi
