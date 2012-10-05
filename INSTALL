#!/bin/sh

# srcdir is the directory where this script resides.
srcdir="$(cd `dirname "$0"` && pwd)"
cd $srcdir

[ ! -x $PLAN9/bin/sed ] && echo 'error: $PLAN9 is not set or does not point to Plan9 tools.' > /dev/stderr && return 1
sed="$PLAN9/bin/sed" # Plan9 sed.

# user can check if we're doing the right thing.
[ "$1" = '-p' ] && pretend=echo

# First part of the path, be it file or directory,
# is appended a dot (.) in the front.  We only copy
# if the name starts with a lowercase letter.

# directories might need to be created before copying files.
echo "`find . -mindepth 1 -maxdepth 1 -type d | 9 grep -v '(/\.)|[A-Z][A-Z]+' | 9 sed 's|^(\./)(.+)$|'$pretend' rm -rf ~/\1.\2|g'`" | sh
echo "`find . -mindepth 1 -maxdepth 1 -type d | 9 grep -v '(/\.)|[A-Z][A-Z]+' | 9 sed 's|^(\./)(.+)$|'$pretend' cp -r \1\2 ~/\1.\2|g'`" | sh
# copy files.
echo "`find . -mindepth 1 -maxdepth 1 -type f | 9 grep -v '(/\.)|[A-Z][A-Z]+' | 9 sed 's|^(.*)/(.+)$|'$pretend' cp \1/\2 ~/\1/.\2|g'`" | sh

