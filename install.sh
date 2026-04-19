#!/bin/sh
#
# Install profile files (including Plan 9 specific files)
# on a Unix system. Also see the rc(1) variant.

set -e

tool=${0##*/}
pretend=false

case $0 in
*/*) script_dir=${0%/*} ;;
*) script_dir=. ;;
esac

# srcdir is the directory where this script resides.
srcdir=$(CDPATH= cd -- "$script_dir" && pwd)

usage() {
	cat <<-END >&2
		# Install profile files on a Unix system.
		USAGE: $tool [-p] [-d dir]
		       $tool          # install files in $HOME
		       $tool -d dir   # install in that directory instead
		       $tool -p       # print what commands $tool would execute
	END
	exit 1
}

run() {
	if $pretend; then
		printf '%s\n' "$*"
	else
		"$@"
	fi
}

instdir=$HOME
while getopts phd: name
do
	case $name in
	p) pretend=true ;;
	d) instdir=$OPTARG ;;
	h|?) usage
	esac
done

run mkdir -p "$instdir"

for f in "$srcdir"/*; do
	[ -f "$f" ] || continue

	b=${f##*/}
	case $b in
	README|make.*) continue ;;
	esac

	run cp "$f" "$instdir/.$b"
done

for d in "$srcdir"/.[!.]* "$srcdir"/..?* "$srcdir"/*; do
	[ -d "$d" ] || continue

	b=${d##*/}
	case $b in
	.git|.|..) continue ;;
	esac

	run cp -R -P "$d" "$instdir"
done
