# This file is Public Domain.

# Makefile included by all other Makefiles.

ifeq ($(OS),)
DOTOS:=$(shell uname | tr A-Z a-z)
endif

ifeq ($(ARCH),)
ifeq ($(OS),darwin)
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
DOTARCH:=${shell if sysctl machdep.cpu.extfeatures | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi}
else
# Ask uname -m for the processor.
DOTARCH:=${shell uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'}
endif
endif

ifeq ($(HOSTNAME),)
HOSTNAME=$(shell hostname | tr A-Z a-z)
endif

# Save for recursive make to avoid recomputing.
export OS ARCH HOSTNAME

