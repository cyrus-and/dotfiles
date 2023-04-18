# common
shopt -s extglob
export EDITOR='emacsclient -c -a emacs'

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

# print the PS1 as a placeholder, temporarily disabling Git PS1 that is part of
# the completions
alias __git_ps1=true
printf "${PS1@P}"
unalias __git_ps1

# swap PS1 for an empty one temporarily
TEMP_PS1="$PS1"
PS1=$'\xe2\x80\x8e' # XXX cannot be empty otherwise the completions are not sourced (LEFT-TO-RIGHT MARK)

# common aliases
alias grep='grep --color=auto'
alias l='ls -F'
alias ll='ls -lartF'
alias gdb='gdb -q'
alias playground='make -sC ~/dev/playground/'

# OS-specific configurations
case "$OSTYPE" in
    darwin*)
        # brew PATH
        export PATH="/usr/local/sbin:$PATH"

        # aliases
        alias ls='ls -G'

        up() {
            (
                set -e
                brew update
                brew upgrade
                brew cleanup
            )
        }

        # append-only bash history
        if [[ -f "$HISTFILE" ]]; then
            chflags uappend "$HISTFILE"
        else
            touch "$HISTFILE"
        fi

        # completion
        source /usr/local/etc/profile.d/bash_completion.sh
        ;;

    'linux-gnu')
        # aliases
        alias ls='ls --color=auto'

        up() {
            (
                set -e
                sudo apt-get update
                sudo apt-get upgrade -y
                sudo apt-get autoremove -y
                sudo apt-get autoclean
                sudo apt-get clean
                sudo updatedb
            )
        }

        # append-only bash history
        if [[ -f "$HISTFILE" ]]; then
            chattr +a "$HISTFILE"
        else
            touch "$HISTFILE"
        fi

        # completions
        source /etc/bash_completion
        ;;
esac

# hides the cursor and reset the cursor position so that the real PS1 overrides
# the placeholder automatically, then alter PS1 to show the cursor every time
printf '\x1b[?25l\x1b[H'
PS1="$TEMP_PS1"$'\\[\x1b[?25h\\]'
unset TEMP_PS1
