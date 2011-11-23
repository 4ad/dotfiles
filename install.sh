#!/bin/sh

MAKE=make
if ! make --version 2>/dev/null | grep 'GNU Make' >/dev/null; then
        MAKE=gmake
fi

find . -mindepth 1 -maxdepth 1 -type d | grep -v hg | xargs "$MAKE" install -C
