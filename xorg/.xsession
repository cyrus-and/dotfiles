# avoid .xsession-errors pollution
exec &>"/tmp/xsession-errors-$USER"

# desktop setup
xset r rate 200 b off mouse 4
feh --bg-center ~/.background &

# non-GUI autostart
LANG=en_US.UTF-8 ~/.dropbox-dist/dropboxd &
emacs --daemon &

# start WM
exec i3
