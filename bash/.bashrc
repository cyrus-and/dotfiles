# path #
PATH=$PATH:~/adt-bundle-linux-x86_64-20130717/sdk/platform-tools:~/adt-bundle-linux-x86_64-20130717/sdk/tools:/opt/jre1.7.0_25/bin/

# go #
# export GOPATH=$HOME/go
# export PATH=$PATH:$GOPATH/bin

# history #
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000000000

# tty #
stty -ctlecho

# aliases #
alias h='history | grep'
alias mydu='du -s * .[^.]* | sort -n'
alias agu='sudo apt-get update'
alias agg='sudo apt-get upgrade -y'
alias agi='sudo apt-get install'
alias agr='sudo apt-get remove'
alias a='apropos'
alias e='emacs -nw'
alias l='ls -lArt'
alias m='man'
alias dired='emacs -nw .'
alias clipboard='xclip -o -selection clipboard'
alias p='python'
alias g='git'
alias noise='aplay -f dat /dev/urandom'

# aux #
function hack {
    local path=$(mktemp -dp ~/tmp "${1:-hack}.XXX")
    cd $path
    emacs . &
}

function al {
    while read -r; do
        echo "<a href='$REPLY'>$REPLY</a>" >> ~/Dropbox/Public/links.html
    done
}

# completions #
. /etc/bash_completion

# local bin
PATH=$PATH:~/bin/

# stash #
export STASH_DIRECTORY=~/Dropbox

# TODO #
# function foo {
#     case "$1" in
#         'mark')
#             (
#                 set -o pipefail
#                 if ! readlink -e "${2:-.}" | stash _foo; then
#                     stash -d _foo
#                     exit 1
#                 fi
#             )
#             ;;
#         'show')
#             stash _foo 2> /dev/null
#             ;;
#         'copy')
#             cp -r "$(stash _foo 2> /dev/null)" "${2:-.}"
#             ;;
#         'move')
#             mv "$(stash _foo 2> /dev/null)" "${2:-.}"
#             ;;
#         *)
#             false
#             ;;
#     esac
# }

# check the terminal type #
if [ "$TERM" != dumb ] ; then
    # colored versions #
    LS_COLORS='di=01;36:ln=00;36'
    export LS_COLORS
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'

    PS1='\[\e[1;30m\]┌─\[$([ $? = 0 ] && X=2 || X=1; tput setaf $X)\]> \[\e[1;36m\]\h\[\e[1;30m\]:\[\e[1;33m\]\w\[\e[0;32m\]$(__git_ps1)\n\[\e[1;30m\]└\[\e[0;35m\]\$\[\e[0m\] '
else
    # plain prompt #
    PS1='\w\n\$ '
fi

# # preexec(command)#
# on_preexec() {
#     # set title #
#     echo -ne "\e]0;$1\007"
#     # print current time #
#     echo -ne "\e[1;30m\e[s\e[2A\e[$[COLUMNS-8]C$(date +%H:%M:%S)\e[u\e[0m"
# }

# preexec() {
#     # TODO avoid screen/tmux?
#     [ -n "$COMP_LINE" ] && return
#     [ "$BASH_COMMAND" = "$PROMPT_COMMAND" ] && return
#     on_preexec "$BASH_COMMAND"
# }

# trap 'preexec' DEBUG
