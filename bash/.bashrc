# history
PROMPT_COMMAND='history -a'
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
HISTIGNORE='fg'

# tty
if [ -t 0 ]; then
    stty -ctlecho -ixon
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

if [[ "$OSTYPE" =~ darwin* ]]; then
    # environment
    export EDITOR='emacs -nw'
    export GIT_EDITOR="$EDITOR"

    # brew PATH
    export PATH="/usr/local/sbin:$PATH"
    export PATH="/usr/local/opt/curl/bin:$PATH"

    # aliases
    alias ls='ls -G'
    alias e="$EDITOR"

    up() {
        brew update  &&\
        brew upgrade &&\
        brew cleanup &&\
        brew cask upgrade &&\
        true
    }

    # completion
    . $(brew --prefix)/etc/bash_completion

    # BUG? -1 not working
    HISTSIZE=100000
fi

if [ "$OSTYPE" = 'linux-gnu' ]; then
    # environment
    export ALTERNATE_EDITOR=''
    export EDITOR='emacsclient -t'
    export GIT_EDITOR="$EDITOR"

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
