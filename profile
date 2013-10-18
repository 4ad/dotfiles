# Don't set -e because we want to login even if sourcing the profile fails.

export OS="`uname | tr A-Z a-z | sed 's/mingw/windows/; s/.*windows.*/windows/'`"
export ARCH="`uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'`"
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
if [ "$OS" = darwin ]; then
	export ARCH="`if sysctl machdep.cpu.extfeatures 2>&1 | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi`"
fi
# Solaris is equally untrustworthy.
if [ "$OS" = sunos ]; then
	export ARCH=`isainfo -n | sed 's/^..86$$/386/; s/^.86$$/386/'`
fi
# Don't use hostname -s, some systems don't support -s; 
# also, some Linux distros don't have hostname.
if [ -x /bin/hostname ]; then
	export H="`/bin/hostname | sed 's/\..*$//'`"
else
	export H=$OS
fi

# Prompt is set here before the Plan 9 tools because they might
# overwrite it for dumb terminals.
PS1='$(printf "%s" "${H}:`basename ${PWD}`$ ")'
# Above prompt doesn't work in zsh, fixed in .zshrc

# Make sure all directories in $PATH exist,
# some tools complain if they don't.
mkdir -p $HOME/bin/$OS/$ARCH
bin=$HOME/bin:$HOME/bin/$OS:$HOME/bin/$OS/$ARCH
# If we're on amd64 and we're not on openbsd, we can
# also run 32 bit binaries.
if [ "$ARCH" = "amd64" -a "$OS" != "openbsd" ]; then
	mkdir -p $HOME/bin/$OS/386
	bin=$bin:$HOME/bin/$OS/386
fi
# Add a local Go to $PATH. This will fail for a system-provided Go,
# that's fine, a later generic check will find it.
goroots="
	$HOME/go
	/usr/local/go
"
for goroot in $goroots; do
	if [ -f $goroot/include/u.h ]; then
		bin=$bin:$goroot/bin
		break
	fi
done
# On SmartOS we want pkgsrc in front of /usr/bin.
if [ "$OS" = sunos ]; then
	[ -d /opt/local/bin ] && bin=$bin:/opt/local/bin
	[ -d /opt/local/sbin ] && bin=$bin:/opt/local/sbin
fi
# If /bin is a symlink (Solaris, some Linux distros, etc), don't
# add it to $PATH.
[ -h /bin ] && defpath=/usr/bin || defpath="/bin /usr/bin"
# Sorted by preference
paths="
	/usr/gnu/bin
	/opt/mse/gcc48/bin
	/opt/omni/bin
	$defpath
	/sbin
	/usr/sbin
	/usr/games
	/usr/sfw/bin
	/usr/local/bin
	/usr/local/sbin
	/usr/pkg/bin
	/usr/pkg/sbin
	/opt/DTT
"
# Add to $PATH if directory exists.
for i in $paths; do
	[ -d $i ] && bin=$bin:$i
done
# It's safe to set $PATH here.
PATH=.:$bin
export CDPATH=.:$HOME

# Check for a working Go.
if [ -x "`which go 2>/dev/null`" ]; then
	export GOPATH=$HOME
	export GOBIN=$HOME/bin/$OS/$ARCH
	goroot=`go env GOROOT`
	cdpaths="$(find $goroot/src/pkg -mindepth 1 -type d | xargs -n1 dirname | sort | uniq)"
	for i in $cdpaths; do
		CDPATH=$CDPATH:$i
	done
	CDPATH=$CDPATH:$goroot/src/cmd
fi
# Add to $CDPATH non-leaf directories from $HOME/src, common names excluded.
if [ -d $HOME/src ]; then
	cdpaths="$(find $HOME/src -mindepth 1 -type d | egrep -v '/(\.)|_[a-zA-Z0-9]' | egrep -v 'bin|cmd|doc|lib|pkg|test' | xargs -n1 dirname | sort | uniq)"
	for i in $cdpaths; do
		CDPATH=$CDPATH:$i
	done
fi

# PAGER is set before the Plan 9 tools because they might
# overwrite it.
if [ -x "`which less 2>/dev/null`" ]; then
	export PAGER=less
	export LESS='-imEQX'
else
	export PAGER=more
	export MORE='-ei'
fi

# Check for Plan 9 tools.
plan9s="
	$HOME/plan9
	/usr/local/plan9
"
for i in $plan9s; do
	if [ -f $i/include/u.h ]; then
		export PLAN9=$i
		break
	fi
done
if [ -n "$PLAN9" ]; then
	PATH=$PATH:$PLAN9/bin

	# Use Anonymous Pro font, if found.
	if [ -f $HOME/lib/font/bit/anonpro/14a/anon.14.font ]; then
		export font=$HOME/lib/font/bit/anonpro/14a/anon.14.font
	else
		export font="$PLAN9/font/luc/unicode.7.font"
	fi
	# On Darwin we want a retina font, but don't enable by default; see r(1).
	if [ "$OS" = darwin ]; then
		export rfont=/mnt/font/Menlo-Regular/22a/font
	fi

	if [ -z "$DISPLAY" ];
	then
		display=:0
	else
		display=$DISPLAY
	fi
	export NAMESPACE=/tmp/ns.$USER.$display
	mkdir -p $NAMESPACE

	_acme() {
		if [ -f $HOME/acme.dump ]; then
			acme -a -l $HOME/acme.dump $*
		else
			acme -a -f $font -F $font $*
		fi
	}
	alias acme=_acme
	alias sam='sam -a'
	_rc() {
		PATH=.:$PLAN9/bin:$bin rc $*
	}
	alias rc=_rc
	
	# Some Plan9 tools work only in X.
	if [ -n "$DISPLAY" ] || [ "$OS" = darwin ] || [ "$termprog" = 9term ] || [ "$termprog" = win ];
	then
		# Plumb files instead of starting new editor.		
		export EDITOR=E
		export FCEDIT=$EDITOR
		export VISUAL=$EDITOR
	
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
	export GS_FONTPATH=$PLAN9/postscript/font

	# Equivalent variables for rc(1).
	export home=$HOME
	export user=$USER
	export prompt="$H% "

	# If running in 9term or acme, make the environment
	# more Plan9 like.
	if [ "$TERM" = 9term -o "$TERM" = dumb ]; then	
		# Disable readline
		set +o emacs
		set +o vi
		# Make man work in 9term and acme's win,
		export PAGER=`which nobs` # Solaris needs full path
		# Set prompt so we can execute whole line
		# without $PS1 interfering.
		PS1='$(printf "%s" ": ${H}:`basename ${PWD}`; ")'
	fi

	# Browsers, in order of preference.
	browsers="
		firefox
		opera
		chromium-browser
		google-chrome
	"
	# Try to set BROWSER (used by the plumber) On darwin, this will fail. 
	# That's fine, we'll use web(1)'s default.
	for i in $browsers; do
		if [ -x "`which $i 2>/dev/null`" ] ; then
			export BROWSER="$i"
		fi
	done

	alias lc='9 lc'
else
	# If we don't have plan9port, perhaps we might have 9base. If we do,
	# we add to the $PATH so sam -r host works.
	if [ -x /usr/local/9/bin/sam ]; then
		PATH=$PATH:/usr/local/9/bin
	fi
fi

# Russ Cox's code search: http://swtch.com/$HOMErsc/regexp/regexp4.html
if [ -x "`which csearch 2>/dev/null`" -a -x "`which cindex 2>/dev/null 2>&1`" ]; then
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
	[ -f $HOME/.csearchindex ] && echo 'cindex >/dev/null 2>&1' | at now >/dev/null 2>&1
fi

# Some aliases.
alias hg10='hg log -l 10'
alias l='ls -F'
alias ls='ls -F'
alias ll='ls -l'
alias la='ls -lA'
alias t='tmux'
alias ta='tmux attach'

# Some shells source $ENV when they're interactive
export ENV=$HOME/.profile

[ -f $HOME/lib/profile.local ] && . $HOME/lib/profile.local
