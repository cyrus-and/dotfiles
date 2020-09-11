# common
shopt -s extglob
export EDITOR=vim

# history
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
HISTIGNORE='fg'
PROMPT_COMMAND='history -a'

# tty
if [ -t 0 ]; then
    stty -ctlecho -ixon
fi

# prompt
if [ "$TERM" != dumb ]; then
    # c <ansi> <text>
    c() { echo -n "\[\e[${2}m\]$1\[\e[0m\]"; }

    PS1="\
$(c '»' '$([ "$?" = 0 ] && echo "1;32" || echo "1;31")')\
$(c '$(jobs | while read; do echo -n "·"; done)' '1;34')\
 \
$(c '\h' '1;36')\
 \
$(c '$([ "$SSH_CONNECTION" ] && echo "ssh " || echo "")' '35')\
$(c '\w' '1;33')\
$(c '$(__git_ps1)' '32')\
\n\
$(c '\$' '35')\
 \
"
    unset c
else
    PS1='\w\n\$ '
fi

# aliases
alias grep='grep --color=auto'
alias l='ls -F'
alias ll='ls -lartF'
alias gdb='gdb -q'
alias playground='make -sC ~/dev/playground/'

# helpers

hack() {
    local path=$(mktemp -dp ~/tmp "hack-${1:-this}.XXX")
    cd $path
    $EDITOR .
}

rttyshell() {
    local PORT="${1:?Specify a port}"
    [ $# = 2 ] && local HOST="-s $2"
    local STTY="$(stty -g)"
    stty -echo raw
    {
        echo exec python -c "\"import pty; pty.spawn(['sh', '-c', 'stty rows $LINES columns $COLUMNS; export TERM=$TERM; clear; exec $SHELL'])\""
        cat
    } | nc -vlp "$PORT" $HOST
    stty "$STTY"
}

fttyshell() {
    local HOST="${1:?Specify a hostname}"
    local PORT="${2:?Specify a port}"
    local STTY="$(stty -g)"
    stty -echo raw
    {
        echo exec python -c "\"import pty; pty.spawn(['sh', '-c', 'stty rows $LINES columns $COLUMNS; export TERM=$TERM; clear; exec $SHELL'])\""
        cat
    } | nc -v "$HOST" "$PORT"
    stty "$STTY"
}

gdb-tmux() {
    local id="$(tmux split-pane -hPF "#D" "tail -f /dev/null")"
    tmux last-pane
    local tty="$(tmux display-message -p -t "$id" '#{pane_tty}')"
    gdb -ex "dashboard -output $tty" "$@"
    tmux kill-pane -t "$id"
}

who-swaps() {
    awk '/^Name:/ { name = $2 } /^VmSwap:/ && $2 != 0 { swap = $2; printf "%dkb %s\n", swap, name }' /proc/+([0-9])/status | column -t | sort -nk1
}

# OS-specific

if [[ "$OSTYPE" =~ darwin* ]]; then
    # mute zsh warning
    export BASH_SILENCE_DEPRECATION_WARNING=1

    # brew PATH
    export PATH="/usr/local/sbin:$PATH"

    # aliases
    alias ls='ls -G'

    up() {
        brew update  &&\
        brew upgrade &&\
        brew cleanup &&\
        true
    }

    # completion
    source $(brew --prefix)/etc/bash_completion

    # BUG? -1 not working
    HISTSIZE=100000
fi

if [ "$OSTYPE" = 'linux-gnu' ]; then
    # aliases
    alias ls='ls --color=auto'

    up() {
        sudo apt-get update        &&\
        sudo apt-get upgrade -y    &&\
        sudo apt-get autoremove -y &&\
        sudo apt-get autoclean     &&\
        sudo apt-get clean         &&\
        sudo updatedb              &&\
        true
    }

    # completions
    source /etc/bash_completion
fi
