# -*- Mode: shell-script; -*-

# Main codebase
export YIPIT_PATH=$HOME/Work/Yipit/yipit
[[ -s "${YIPIT_PATH}/conf/yipit_bash_profile" ]] && source "${YIPIT_PATH}/conf/yipit_bash_profile"

# Chef
export YIPIT_BOX=Yipit12.04.1-10.16.2
export CHEF_PATH=$HOME/Work/Yipit/yipit-chef
source ${CHEF_PATH}/settings/yipit_chef_functions
