PATH=.:~/bin:$HOME/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

PS1='$(
    [[ "${LOGNAME}" == "root" ]] && printf "%s" "${HOSTNAME}:${PWD/${HOME}/~}# " ||
    printf "%s" "${HOSTNAME}:${PWD/${HOME}/~}\$ ")'

export PLAN9=$HOME/plan9
PATH=$PATH:$PLAN9/bin

export font="$PLAN9/font/luc/unicode.7.font"
alias acme="acme -a -f $font -F $PLAN9/font/fixed/unicode.8x13.font -l ~/acme.dump"
alias sam='sam -a'

alias ls='ls -F'
alias ll='ls -lA'
