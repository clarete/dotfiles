# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force ignoredups and ignorespace
HISTCONTROL=ignoreboth

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

# I want to know when things break!
ulimit -c unlimited

# General stuff
export PS1='\[\033[01;32m\]\u@yipster\[\033[01;34m\] \w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '

# Python
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Yipit
export YIPIT_PATH=$HOME/Work/Yipit/yipit
source ${YIPIT_PATH}/conf/yipit_bash_profile

# Ruby stuff, Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Setting the path
PATH="$HOME/bin:$PATH"
export PATH
