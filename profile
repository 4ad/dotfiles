# Don't set -e because we want to be able to login even if sourcing
# the profile fails.

os=$(uname -s)

# Sorted by preference. GHCup has to come before Cabal.
paths_user="
	${HOME}/bin
	${HOME}/.local/bin
	${HOME}/.cargo/bin
	${HOME}/.ghcup/bin
	${HOME}/.cabal/bin
	${HOME}/go/bin
"

# On SmartOS we want pkgsrc in front of /usr/bin.
if [ "$os" = "SunOS" ]; then
	paths_smartos="
		/opt/local/bin
		/opt/local/sbin
		/opt/tools/bin
		/opt/tools/sbin
	"
fi

# Sorted by preference. On macOS path_helper(8) might reset
# the order, that's fine.
paths_sys="
	/proc/boot
	${paths_smartos}
	/usr/sbin
	/usr/bsd
	/usr/bin
	/sbin
	/bin
	/snap/bin
	/usr/gnu/bin
	/usr/ccs/bin
	/usr/dt/bin
	/usr/openwin/bin
	/usr/X11R7/bin
	/usr/X11R6/bin
	/usr/X11/bin
	/opt/X11/bin
	/usr/bin/X11
	/usr/X/bin
	/usr/ucb
	/usr/games
	/Library/Apple/usr/bin
	/opt/bin
	/opt/DTT
	/opt/DTT/bin
	/usr/local/go/bin
	/usr/sfw/bin
	/usr/pkg/bin
	/usr/pkg/sbin
	/opt/local/bin
	/opt/local/sbin
	/usr/local/bin
	/usr/local/sbin
	/usr/local/games
	/opt/homebrew/bin
	/opt/homebrew/sbin
	/opt/ooce/bin
	/opt/sfw/bin
	/opt/freeware/bin
	/usr/freeware/bin
	/usr/nekoware/bin
	/opt/sw
"

for i in $paths_user $paths_sys; do
	# Add to $PATH if directory exists and is not a symlink.
	# This avoids duplicate PATH entries on systems where
	# /bin is a symlink to /usr/bin (Solaris, modern Linux, etc).
	[ -d "$i" ] && [ ! -h "$i" ] && bin="${bin:+$bin:}$i"
done

# It's safe to set $PATH here.
export PATH=$bin

# Run path_helper(8), if available. This is macOS specific.
# It adds macOS cryptexes to the PATH as well as .pkg packages
# that installed their path in /etc/paths.d.
if [ -x /usr/libexec/path_helper ]; then
	eval "$(/usr/libexec/path_helper -s)"
fi

# GOPATH defaults to $HOME/go, but we don't want that because
# there could be a local Go install there.
export GOPATH=$HOME

# Check for a local OCaml. This has to happen after setting PATH.
if [ -r $HOME/.opam/opam-init/init.sh ]; then
	. $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

# Check for Plan 9 tools.
plan9_dirs="
	${HOME}/plan9
	/usr/local/plan9
"
for i in $plan9_dirs; do
	if [ -f "$i/include/u.h" ]; then
		export PLAN9=$i
		break
	fi
done
if [ -n "$PLAN9" ]; then
	PATH=$PATH:$PLAN9/bin

	display=${DISPLAY:-:0}
	nsdisplay=$(printf '%s' "$display" | tr '/' '_')
	export NAMESPACE=/tmp/ns.$USER.$nsdisplay
	mkdir -p "$NAMESPACE"

	if fontsrv -p LucidaGrandeMono >/dev/null 2>&1; then
		export font=/mnt/font/LucidaGrandeMono/14a/font
	fi

	export GS_FONTPATH=$PLAN9/postscript/font

	# Equivalent variables for rc(1).
	export home=$HOME
	export user=$USER
else
	# If we don't have plan9port, perhaps we might have 9base. If we do,
	# we add to the $PATH so sam -r host works.
	if [ -x /usr/lib/plan9/bin/sam ]; then
		PATH=$PATH:/usr/lib/plan9/bin
	fi
fi

# Prevent newer macOS systems from admonishing me.
export BASH_SILENCE_DEPRECATION_WARNING=1

[ -f $HOME/lib/profile.local ] && . $HOME/lib/profile.local

# Some shells source $ENV when they're interactive
export ENV=$HOME/.rc
