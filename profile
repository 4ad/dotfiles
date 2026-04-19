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
	${paths_smartos}
	/usr/sbin
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
	/usr/ucb
	/usr/games
	/Library/Apple/usr/bin
	/opt/DTT
	/opt/DTT/bin
	/usr/local/go/bin
	/usr/sfw/bin
	/usr/pkg/bin
	/usr/pkg/sbin
	/usr/local/bin
	/usr/local/sbin
	/usr/local/games
	/opt/homebrew/bin
	/opt/homebrew/sbin
	/opt/ooce/bin
	/opt/sfw/bin
"

# unset these so this script is idempotent.
unset bin path_entries
path_entries=
for i in $paths_user $paths_sys; do
	[ -d "$i" ] || continue

	seen=false
	old_ifs=$IFS
	IFS=:
	for j in $path_entries; do
		# Non-POSIX, but supported everywhere we care about.
		if [ "$i" = "$j" ] || /bin/test "$i" -ef "$j"; then
			seen=true
			break
		fi
	done
	IFS=$old_ifs
	$seen && continue

	bin="${bin:+$bin:}$i"
	path_entries="${path_entries:+$path_entries:}$i"
done
unset i j seen old_ifs path_entries

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
# We only want opam's environment here, not its interactive shell hooks.
if [ -r $HOME/.opam/opam-init/variables.sh ]; then
	# opam initialization bails out if OPAM_SWITCH_PREFIX is
	# already set. Since we reset the environment from scratch
	# clear inherited opam state so a fresh login shell reconstructs
	# a consistent default opam environment.
	unset OPAM_LAST_ENV OPAMROOT OPAMROOTISOK OPAMSWITCH OPAM_SWITCH_PREFIX
	unset OCAMLTOP_INCLUDE_PATH CAML_LD_LIBRARY_PATH OCAML_TOPLEVEL_PATH
	. $HOME/.opam/opam-init/variables.sh > /dev/null 2> /dev/null || true
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
	export NAMESPACE=/tmp/ns.$LOGNAME.$nsdisplay
	mkdir -p "$NAMESPACE"

	if fontsrv -p LucidaGrandeMono >/dev/null 2>&1; then
		export font=/mnt/font/LucidaGrandeMono/14a/font
	fi

	export GS_FONTPATH=$PLAN9/postscript/font

	# Equivalent variables for rc(1).
	export home=$HOME
	export user=$LOGNAME
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

# Some shells source $ENV when they're interactive.
export ENV=$HOME/.shinit
