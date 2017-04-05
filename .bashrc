# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force ignoredups and ignorespace
HISTCONTROL=ignoreboth
export HISTSIZE=1000

# Timestamps in history file
export HISTTIMEFORMAT="%d/%m/%y %T "

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS.
shopt -s checkwinsize

# Alias definitions.
if [ -f ~/.bash.d/aliases ]; then
    . ~/.bash.d/aliases
fi

# I want to know when things break!
ulimit -c unlimited

# General stuff
export EMAIL='lincoln@comum.org'
export EDITOR="emacsc"

# Debian variables
export DEBEMAIL='lincoln@comum.org'
export DEBFULLNAME='Lincoln Clarete'

# Tell chromium its fine to load extensions
export CHROMIUM_FLAGS='--enable-remote-extensions'

# PS1, yeah it's a big deal!
black=$(tput setaf 0)
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
bold=$(tput bold)
reset=$(tput sgr0)

function __venv_name() {
    [ -n "$VIRTUAL_ENV" ] && echo -n "($(basename $VIRTUAL_ENV))"
}

# Since the venv name written in PS1 by the virtualenv's postactivate
# script won't have colors, the original customized value is saved in
# the variable `ORIG_PS1` and re-exported in the postactivate script.
export ORIG_PS1="\[$yellow\]\$(__venv_name) \[$green\]\$(__git_ps1 \"%s \")\[$red\]♡ \[$reset\]\[$bold\]\W $ \[$reset\]"
export PS1=$ORIG_PS1

# Extend path with reasonable directories
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$HOME/.localemacs/bin:$PATH"
export PATH

# Load platform specific settings
platform=$(uname 2>/dev/null | tr '[A-Z]' '[a-z]')
. $HOME/.bash.d/platform/$platform

# Load programming framework specific settings
. $HOME/.bash.d/lang/go
. $HOME/.bash.d/lang/nodejs
. $HOME/.bash.d/lang/python
. $HOME/.bash.d/lang/ruby
. $HOME/.bash.d/lang/android
