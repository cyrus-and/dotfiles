# avoid .xsession-errors pollution
exec 1> /dev/null 2> /dev/null

# setup
{ feh --bg-center .background ; xcompmgr -n ; } &
conky &
xset r rate 200 50 b off mouse 4 &

# wm
openbox-session &
WM=$!

# delay
sleep 1

# autostart
while urxvt -name root-terminal -title root-terminal -e tmux attach; do :; done &
dropbox start &
transmission-daemon &
google-chrome &

# wait
wait $WM
