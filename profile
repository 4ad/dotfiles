# Source system wide profile first so we don't pollute
# the environment if we source this file multiple times.
. /etc/profile

OS="`uname | tr A-Z a-z | sed 's/mingw/windows/; s/.*windows.*/windows/'`"

ARCH="`uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'`"
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
[ "$OS" = "darwin" ] && ARCH="`if sysctl machdep.cpu.extfeatures 2>&1 | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi`"

# Some Linux distros don't have hostname, amazing.
[ -x /bin/hostname ] && HOSTNAME="`hostname`"

export OS ARCH HOSTNAME

# Make sure all directories in $PATH exist,
# some tools complain if they don't.
mkdir -p ~/bin/$OS/$ARCH
BIN=.:~/bin:~/bin/$OS:~/bin/$OS/$ARCH

# Check for Go.
[ -f ~/go/include/u.h ] && BIN=$BIN:~/go/bin

PATH=$BIN:$PATH

# Check for Plan9 tools.
if [ -f ~/plan9/include/u.h ]; then
	export PLAN9="~/plan9"
	PATH=$PATH:$PLAN9
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

PATH=.:~/bin:$HOME/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

PS1='$(
    [[ "${LOGNAME}" == "root" ]] && printf "%s" "${HOSTNAME}:${PWD/${HOME}/~}# " ||
    printf "%s" "${HOSTNAME}:${PWD/${HOME}/~}\$ ")'

export PLAN9=$HOME/plan9
PATH=$PATH:$PLAN9/bin

export font="$PLAN9/font/luc/unicode.7.font"
alias acme="acme -a -f $font -F $PLAN9/font/fixed/unicode.8x13.font -l ~/acme.dump"
alias sam='sam -a'

alias ls='ls -F'
alias ll='ls -lA'
