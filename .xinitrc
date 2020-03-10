# Load some X configuration
xrdb -merge /home/lincoln/.Xresources

# Dead key support so I can write in pt-br correctly
setxkbmap us alt-intl

# EXWM ftw.
exec /usr/bin/dbus-launch --exit-with-session emacs
