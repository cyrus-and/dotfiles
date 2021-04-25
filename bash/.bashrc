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
if [[ -t 0 ]]; then
    stty -ctlecho -ixon
fi

# prompt
if [[ "$TERM" != dumb ]]; then
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

# OS-specific

if [[ "$OSTYPE" =~ darwin* ]]; then
    # brew PATH
    export PATH="/usr/local/sbin:$PATH"

    # aliases
    alias ls='ls -G'

    up() {
        brew update  && \
        brew upgrade && \
        brew cleanup && \
        true
    }

    # completion
    source $(brew --prefix)/etc/bash_completion
fi

if [[ "$OSTYPE" = 'linux-gnu' ]]; then
    # aliases
    alias ls='ls --color=auto'

    up() {
        sudo apt-get update        && \
        sudo apt-get upgrade -y    && \
        sudo apt-get autoremove -y && \
        sudo apt-get autoclean     && \
        sudo apt-get clean         && \
        sudo updatedb              && \
        true
    }

    # completions
    source /etc/bash_completion
fi
