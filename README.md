# Abstract

Dotfiles for a wide variety of Unix and Unix-like systems that
include plan9port support. You probably don't want this.

# Installation instructions

    ./install.sh                   # if POSIX /bin/sh
    /usr/xpg4/bin/sh install.sh    # pre-POSIX SVR Bourne /bin/sh

Install script supports -p (pretend) and -d dir (install in dir).

# Platform support and requirements

These files require a POSIX compliant user shell. In addition they
require `/bin/test` to be present and support `-ef` which is not
in POSIX.

Supported operating systems:
- Linux (any distribution except NixOS).
- macOS
- FreeBSD
- OpenBSD, NetBSD
- Solaris 8/9/10 (only with ksh shell or better, not SVR `/bin/sh`)
- Solaris 11
- illumos (SmartOS, OmniOSce, OpenIndiana)
- Minix 3

Other systems may or may not work.

Supported shells:
- bash 3.2.57       (latest version shipped with macOS)
- bash              (modern versions)
- ksh93
- ash
- dash              (NetBSD ash derivative)
- FreeBSD `/bin/sh`   (ash derivative)
- NetBSD `/bin/sh`    (ash derivative)
- OpenBSD `/bin/ksh`  (pdksh derivative)
- yash              (maximally POSIX-compliant shell)
- zsh

Pre-POSIX SVR Bourne shells (like `/bin/sh` in Solaris 8/9/10) are
not supported.

# License

Public domain.
