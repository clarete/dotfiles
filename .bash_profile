# My login shell starts a screen session. When screen forks a bash
# process, it must be configured to *not* start it as a login shell,
# otherwise a loop will take place.
#
# PS.: Configure gnome terminal, iTerm, etc to 'Run command as a login
# shell' to run this file.
exec screen
