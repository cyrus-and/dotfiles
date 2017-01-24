# avoid .xsession-errors pollution
exec &>"/tmp/xsession-errors-$USER"

# desktop setup
xset r rate 200 50 b off mouse 4
feh --bg-center ~/.background &

# disable keys to fix bad behaviors
xmodmap -e 'keycode 110=' # home
xmodmap -e 'keycode 115=' # end

# non-GUI autostart
LANG=en_US.UTF-8 ~/.dropbox-dist/dropboxd &

# start WM
exec i3
