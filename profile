OS="`uname | tr A-Z a-z | sed 's/mingw/windows/; s/.*windows.*/windows/'`"

ARCH="`uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'`"
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
[ "$OS" = darwin ] && ARCH="`if sysctl machdep.cpu.extfeatures 2>&1 | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi`"
# Solaris is equally untrustworthy
[ "$OS" = sunos ] && ARCH=`isainfo -n | sed 's/^..86$$/386/; s/^.86$$/386/'`

# Don't use hostname -s, some systems don't have it; 
# also, some Linux distros don't have hostname.
[ -x /bin/hostname ] && H="`/bin/hostname | sed 's/\..*$//'`" || H=$OS

export OS ARCH H

# Make sure all directories in $PATH exist,
# some tools complain if they don't.
mkdir -p ~/bin/$OS/$ARCH
export BIN=~/bin:~/bin/$OS:~/bin/$OS/$ARCH
export CDPATH=.:~

# If we're on amd64 and we're not on openbsd, we can
# also run 32 bit binaries.
if [ "$ARCH" = "amd64" -a "$OS" != "openbsd" ]; then
	mkdir -p ~/bin/$OS/386
	BIN=$BIN:~/bin/$OS/386
fi

# Check for Go.
if [ -f ~/go/include/u.h ]; then
	BIN=$BIN:~/go/bin
	export GOPATH=~
	alias gi='go install'
	alias gb='go build'
	alias gt='go test'

	# Add to $CDPATH non-leaf go packages.
	cdpaths="$(find ~/go/src/pkg -mindepth 1 -type d | egrep -v '/(\.)|_[a-zA-Z0-9]' | xargs -n1 dirname | sort | uniq)"
	for i in $cdpaths; do
		CDPATH=$CDPATH:$i
	done
	CDPATH=$CDPATH:~/go/src/cmd
fi

# Check for App Engine.
if [ -x ~/google_appengine/appcfg.py ]; then
	BIN=$BIN:~/google_appengine
	alias app=dev_appserver.py
	alias appcfg=appcfg.py
fi

# Check for 9vx
if [ -f ~/9vx/bin/9vx ] && [ -f ~/nix-os/sys/include/9p.h ]; then
	BIN=$BIN:~/9vx/bin
	export PLAN9VXROOT=~/nix-os
fi

# If /bin is a symlink (some UNICES), don't add it to $PATH
[ ! -h /bin ] && BIN=$BIN:/bin
# Sorted by preference
paths="
	/opt/local/bin	# Joyent Pkgsrc
	/opt/local/sbin	# Joyent Pkgsrc
	/usr/gnu/bin	# Solaris
	/usr/bin
	/sbin
	/usr/sbin
	/usr/games
	/usr/sfw/bin	# Solaris stuff
	/usr/local/bin	# FreeBSD ports and local Unix stuff
	/usr/local/sbin # FreeBSD ports and local Unix stuff
	/usr/pkg/bin	# NetBSD ports
	/usr/pkg/sbin	# NetBSD ports
	/opt/DTT	# SmartOS DTrace toolkit
"
# Add to $PATH if directory exists.
for i in $paths; do
	[ -d $i ] && BIN=$BIN:$i
done

# It's safe to set $PATH here.
PATH=.:$BIN

# Add to $CDPATH non-leaf directories from ~/src, common names excluded.
cdpaths="$([ -d ~/src ] && find ~/src -mindepth 1 -type d | egrep -v '/(\.)|_[a-zA-Z0-9]' | egrep -v 'bin|cmd|doc|lib|pkg|test' | xargs -n1 dirname | uniq)"
for i in $cdpaths; do
	CDPATH=$CDPATH:$i
done

# PAGER is set before the Plan9 tools because they might
# overwrite it.
if [ -x /bin/less ]; then
	export PAGER=less
	export LESS='-imQX'
else
	export PAGER=more
	export MORE='-ei'
fi

# Check for Plan9 tools.
plan9s="
	/usr/local/plan9
	$HOME/plan9
"
for i in $plan9s; do
	if [ -f $i/include/u.h ]; then
		PLAN9=$i
	fi
done

if [ -f /usr/local/plan9/include/u.h ]; then
	PLAN9=/usr/local/plan9
fi
if [ -f ~/plan9/include/u.h ]; then
	PLAN9=~/plan9
fi
if [ $PLAN9 != "" ]; then
	PATH=$PATH:$PLAN9/bin

	# Use Anonymous Pro font, if found.
	if [ -f ~/.fonts/plan9/anonpro/14a/anon.14.font ]; then
		font=~/.fonts/plan9/anonpro/14a/anon.14.font
	else
		font="$PLAN9/font/luc/unicode.7.font"
	fi

	_acme() {
		if [ -f ~/acme.dump ]; then
			acme -a -f $font -F $font -l ~/acme.dump "$@"
		else
			acme -a -f $font -F $font "$@"
		fi
	}
	alias acme=_acme
	alias sam='sam -a'
	_rc() {
		PATH=.:$PLAN9/bin:$BIN rc "$@"
	}
	alias rc=_rc
	
	# Some Plan9 tools work only in X.
	if [ -n "$DISPLAY" ] || [ "$OS" = "darwin" ] || [ "$TERM" = "9term" ];
	then
		# Plumb files instead of starting new editor.		
		EDITOR=E
		FCEDIT=$EDITOR
		VISUAL=$EDITOR
	
		# Keep the label up to date, so plumber works
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

	# Let gs find the plan9port document fonts.
	GS_FONTPATH=$PLAN9/postscript/font

	# Equivalent variables for rc(1).
	home=$HOME
	user=$USER
	prompt="$H% "

	# If running in 9term or acme, make the environment
	# more Plan9 like.
	if [ "$TERM" = "9term" ]; then	
		# Disable readline
		set +o emacs
		set +o vi
		
		# Enable autoexport, like in rc(1).
		set -a
		
		# Make man work in 9term and acme's win,
		PAGER=nobs
	fi

	export PLAN9 font EDITOR FCEDIT VISUAL GS_FONTPATH home user prompt PAGER
else
	# If we don't have plan9port, perhaps we might have 9base. If we do,
	# we add to the $PATH so sam -r host works.
	if [ -x /usr/local/9/bin/sam ]; then
		PATH=$PATH:/usr/local/9/bin
	fi
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
PS1='$(printf "%s" "${H}:`basename ${PWD}`$ ")'
# Above prompt doesn't work in zsh, fixed in .zshrc

# Some aliases.
alias bz10='bzr log --line -r -10..-1'
alias bztip='bzr log --line -r -1'
alias bzlog='bzr log --line'
alias bzd='bzr diff'
alias hg10='hg log -l 10'
alias l='ls -F'
alias lc='9 lc'
alias ls='ls -F'
alias ll='ls -l'
alias la='ls -lA'
alias t='tmux'
alias ta='tmux attach'
alias xc='xmonad --recompile'
# Russ Cox' code search: http://swtch.com/~rsc/regexp/regexp4.html
if [ -x "`which csearch >/dev/null 2>&1`" ] && [ -x "`which cindex >/dev/null 2>&1`" ]; then
	# Filter paths so it's relative to `pwd`.
	_filterPath () {
		case "`pwd`" in
		/)
			cat
			;;
		*)
			sed "s|`pwd`/||"
			;;
		esac
	}
	_cs () {
		csearch -n "$@" | _filterPath
	}
	alias cs=_cs
	_csh () {
		csearch -n -f `pwd` "$@" | _filterPath
	}
	alias csh=_csh
	alias ci='cindex'
	# if we have an index, update it at login.
	[ -f ~/.csearchindex ] && echo 'cindex >/dev/null 2>&1' | at now >/dev/null 2>&1
fi

# Some shells source $ENV when they're interactive
export ENV=~/.profile
