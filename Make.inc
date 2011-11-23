# This file is Public Domain.

# Makefile included at the end of all other Makefiles.

ifeq ($(OS),)
OS:=$(shell uname | tr A-Z a-z)
endif

ifeq ($(ARCH),)
ifeq ($(OS),darwin)
# Even on 64-bit platform, darwin uname -m prints i386.
# Check for amd64 with sysctl instead.
ARCH:=${shell if sysctl machdep.cpu.extfeatures | grep EM64T >/dev/null; then echo amd64; else uname -m | sed 's/i386/386/'; fi
else
# Ask uname -m for the processor.
ARCH:=${shell uname -m | sed 's/^..86$$/386/; s/^.86$$/386/; s/x86_64/amd64/; s/arm.*/arm/'}
endif
endif

ifeq ($(HOSTNAME),)
HOSTNAME=$(shell hostname | tr A-Z a-z)
endif

# Save for recursive make to avoid recomputing.
export OS ARCH HOSTNAME

# Makefiles may override this so we don't export it.
ifeq ($(FINALDIR),)
FINALDIR:=$(HOME)
endif

# TARG is the only variable required to be set by makefiles.
ifeq ($(TARG),)
$(error $$TARG is not set; each Makefile must set $$TARG)
endif

# Final name for the dotfile.
# Makefiles may override this so we don't export it.
ifeq ($(FINALTARG),)
FINALTARG:=.$(TARG)
endif

# Prepare files here before installing in user directory
DIR=$(shell pwd)/bin

dir:
	mkdir -p $(DIR)

# Default rule copies the base file IFF it exists.
# Makefiles add to this rule if the target needs processing.
# The default rule doesn't create an empty file so that the user
# receives an error if the target lacks required processing.
$(DIR)/$(TARG):: dir
	test -f $(TARG) && cp $(TARG) $(DIR)

# These rules assume OS/ARCH/HOST specific files are not
# dynamically generated and just copy them or create
# empty stubs.
$(DIR)/$(TARG).$(OS): dir
	touch $(DIR).$(TARG).$(OS)
	test -f $(TARG).$(OS) && cp $(TARG).$(OS) $(DIR)
	
$(DIR)/$(TARG).$(ARCH): dir
	touch $(DIR).$(TARG).$(ARCH)
	test -f $(TARG).$(ARCH) && cp $(TARG).$(ARCH) $(DIR)

$(DIR)/$(TARG).$(HOSTNAME): dir
	touch $(DIR).$(TARG).$(HOSTNAME)
	test -f $(TARG).$(HOSTNAME) && cp $(TARG).$(HOSTNAME) $(DIR)

# This rule assembles the final file.  It explicitly lists $(DIR)/$(TARG)
# because it may be dynamically generated.
$(DIR)/$(TARG).$(OS).$(ARCH).$(HOSTNAME): $(DIR)/$(TARG) $(DIR)/$(TARG).$(OS) $(DIR)/$(TARG).$(ARCH) $(DIR)/$(TARG).$(HOSTNAME) 
	cat $(DIR)/$(TARG) $(DIR)/$(TARG).$(OS) $(DIR)/$(TARG).$(ARCH) $(DIR)/$(TARG).$(HOSTNAME) > $(DIR)/$(TARG).$(OS).$(ARCH).$(HOSTNAME)

all: $(DIR)/$(TARG).$(OS).$(ARCH).$(HOSTNAME)

install: all
	cp $(DIR)/$(TARG).$(OS).$(ARCH).$(HOSTNAME) $(FINALDIR)/$(FINALTARG)
	
clean:
	rm -f $(DIR) >/dev/null 2>&1

.PHONY:	dir all clean install
