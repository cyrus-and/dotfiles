# common
shopt -s extglob

# history
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=-1
HISTFILESIZE=-1
HISTIGNORE='fg'
PROMPT_COMMAND='history -a'

# tty
if [[ -t 0 ]]; then
    stty -ctlecho
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

# common aliases
alias ls='ls --color=auto' # assuming GNU utils
alias grep='grep --color=auto'
alias ll='ls -lartF'

# OS-specific configurations
case "$OSTYPE" in
    darwin*)
        # brew PATH
        export PATH="/usr/local/sbin:$PATH"

        # XXX macOS 12.3 or earlier
        alias dropbox-ignore='xattr -w com.dropbox.ignored 1'
        alias dropbox-unignore='xattr -d com.dropbox.ignored 1'
        alias dropbox-check='xattr -p com.dropbox.ignored'

        up() {
            (
                set -e
                nix-env --upgrade
            )
        }

        # append-only bash history
        if [[ -f "$HISTFILE" ]]; then
            chflags uappend "$HISTFILE"
        else
            touch "$HISTFILE"
        fi

        # completion
		source ~/.nix-profile/share/bash-completion/bash_completion
		source ~/.nix-profile/share/bash-completion/completions/git-prompt.sh
        ;;

    'linux-gnu')
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

# configurations

# password store on dropbox
export PASSWORD_STORE_DIR="$HOME/Dropbox/root/pass/"

# ZFZ integration
eval "$(fzf --bash)"
