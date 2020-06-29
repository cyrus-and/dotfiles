# avoid .xsession-errors pollution
exec &>"/tmp/xsession-errors-$USER"

# desktop setup
xset s off
xset -dpms
xset s noblank
xset r rate 200 b off
feh --bg-center ~/.background &
redshift -l geoclue2 &

# non-GUI autostart
~/.dropbox-dist/dropboxd &
emacs --daemon &

# start WM
exec i3
