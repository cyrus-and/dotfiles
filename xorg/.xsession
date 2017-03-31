# avoid .xsession-errors pollution
exec &>"/tmp/xsession-errors-$USER"

# desktop setup
xset r rate 200 b off # mouse 4
# xinput --set-prop 8 'libinput Accel Speed' 1
feh --bg-center ~/.background &

# non-GUI autostart
~/.dropbox-dist/dropboxd &
emacs --daemon &

# start WM
exec i3
