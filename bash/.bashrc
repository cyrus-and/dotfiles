# environment
export EDITOR='emacsclient -t'
export GIT_EDITOR="$EDITOR"

# history
PROMPT_COMMAND='history -a'
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
HISTIGNORE='fg'

# tty
if [ -t 0 ]; then
    stty -ctlecho
    stty -ixon
fi

# prompt
if [ "$TERM" != dumb ]; then
    # c <ansi> <text>
    c() { echo -n "\[\e[${2}m\]$1\[\e[0m\]"; }

    PS1="\
$(c '*' '$([ "$?" = 0 ] && echo "1;32" || echo "1;31")')\
$(c '$(jobs | while read; do echo -n "·"; done)' '1;34')\
 \
$(c '\h' '1;36')\
 \
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
alias mydu='du -s * .[^.]* | sort -n'
alias l='ls -lArt'
alias p='python3'
alias gdb='gdb -q'

# helpers

hack() {
    local path=$(mktemp -dp ~/tmp "hack-${1:-this}.XXX")
    cd $path
    e .
}

# OS-specific

if [ "$OSTYPE" = 'darwin16' ]; then
    # brew PATH
    export PATH="/usr/local/sbin:$PATH"

    # GPG agent
    gpg_agent_file="$HOME/.gpg-agent-info"
    if ! killall -0 gpg-agent &>/dev/null; then
        gpg-agent --daemon --write-env-file "$gpg_agent_file" >/dev/null
    fi
    . "$gpg_agent_file"
    export GPG_AGENT_INFO

    # aliases
    alias ls='ls -G'
    alias e="$EDITOR"

    up() {
        brew update  &&\
        brew upgrade &&\
        brew cleanup &&\
        true
    }

    # completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi

    # BUG? -1 not working
    HISTSIZE=100000
fi

if [ "$OSTYPE" = 'linux-gnu' ]; then
    # paths
    export GOPATH=$HOME/go/
    export PATH=$PATH:$GOPATH/bin/

    # aliases
    alias ls='ls --color=auto'
    alias dmesg='dmesg -w'
    alias xcopy='xclip -i -selection clipboard'
    alias xpaste='xclip -o -selection clipboard'
    alias emacs='emacsclient -c -a emacs'
    alias e='emacs -nw'

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
