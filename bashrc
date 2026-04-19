# In some cases Bash will source this file even if the shell
# is not interactive. This happens if we run non-interactive
# commands using ssh(1), SHELL is /bin/bash, and Bash has been
# compiled with SSH_SOURCE_BASHRC (default on Debian, Ubuntu,
# and Fedora).
#
# Work around this misfeature.
[[ $- != *i* ]] && return

[[ -f $HOME/.shrc ]] && . $HOME/.shrc

if command -v direnv >/dev/null 2>&1; then
	eval "$(direnv hook bash)"
fi
