# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force ignoredups and ignorespace
HISTCONTROL=ignoreboth
export HISTSIZE=1000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Alias definitions.
if [ -f ~/.bash.d/aliases ]; then
    . ~/.bash.d/aliases
fi

# Loading specific things depending on our platform
[ "`uname`" = "Darwin" ] && . $HOME/.bash.d/mac
[ "`uname -o 2>/dev/null`" = "GNU/Linux" ] && . $HOME/.bash.d/gnu-linux

# I want to know when things break!
ulimit -c unlimited

# General stuff
export EMAIL='lincoln@comum.org'
export EDITOR="emacsc"

# PS1, yeah it's a big deal!
function __ps1() {
    echo -en "\033[00;33m"
    [ -n "$VIRTUAL_ENV" ] && echo -n "($(basename $VIRTUAL_ENV)) "

    echo -en "\033[00;32m"
    local git=$(__git_ps1 "%s")
    [ -n "$git" ] && echo -n "$git "
    echo -en "\033[31;1m♡ \033[01;30m$(basename $(pwd)) $\033[00m "
}

# This same line is repeated in the file ~/.virtualenvs/postactivate
export PS1='$(__ps1)'

# Pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
pyenv virtualenvwrapper

# Adding my custom path directory
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"

# Ruby stuff, Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Go
export GOPATH="${HOME}/Work/go"
PATH="${PATH}:${GOPATH}/bin"

# Exporting the PATH before loading the custom scripts.
export PATH

# Loading custom scripts
for i in $HOME/.bash.d/custom.d/*.sh; do . $i; done

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
