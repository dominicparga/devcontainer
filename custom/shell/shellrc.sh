#!/usr/bin/env sh

#------------------------------------------------------------------------------#
# If not running interactively, don't do anything

case $- in
    *i*) ;;
      *) return ;;
esac

#------------------------------------------------------------------------------#
# setup

export DOTFILES="$HOME/workspace/dotfiles/."

. "${DOTFILES}/shell/shellrc.sh"

greet

#------------------------------------------------------------------------------#

# ----------------------------------------------------
# personal

# use emacs keybindings
if [[ -n "${ZSH_NAME}" ]]; then
    bindkey -e

    autoload -U bashcompinit
    bashcompinit
    AOS_BASH_COMPLETION="$HOME/.bash_aos_completion"
    if [[ -f "${AOS_BASH_COMPLETION}" ]]; then
        . "${AOS_BASH_COMPLETION}"
    fi
fi

# Start gnome keyring
if [ -n "$DESKTOP_SESSION" ];then
    eval "$(gnome-keyring-daemon --start)"
    export SSH_AUTH_SOCK
else
    # start ssh agent for remote sessions and add personal certificates to
    # prevent repeated password input
    . "${HOME}/.ssh-find-agent"
    ssh_find_agent -a
    if [ -z "$SSH_AUTH_SOCK" ]
    then
        eval $(ssh-agent) > /dev/null
        ssh-add
    fi
fi

# fixes fancy prompt issues when called from remote modules like emacs
if [[ $TERM == "dumb" ]]; then
    export PS1="$ "
fi

alias la='ls -altrh'
alias cnt='ls -F |grep -v / | wc -l'

grep_find() {
  find . -type f -exec grep -i "$1" {} +
}

untar() {
  tar -xvzf $1
}

# use vim as SVN editor
export SVN_EDITOR=vim
export GIT_EDITOR=vim
export GIT_LFS_SKIP_SMUDGE=1

# expand path to include local bin directory
PATH=$HOME/opt/bin:$HOME/.local/bin:$PATH

# expand path to include newest cmake version
PATH=/usr/local/cmake/3.18.4/bin:$PATH

# moving files to trash from command line
alias "trash"="gvfs-trash"

alias dfs="hdfs dfs"

export EMACS="emacsclient -c -a emacs %f"

#------------------------------------------------------------------------------#
# ros setup

if [[ -n "${ZSH_NAME}" ]]; then
    # melodic is only for Ubuntu 18.04
    if [[ -f /opt/ros/melodic/setup.zsh ]]; then
        . /opt/ros/melodic/setup.zsh
    fi
elif [[ -n "${BASH}" ]]; then
    # melodic is only for Ubuntu 18.04
    if [[ -f /opt/ros/melodic/setup.bash ]]; then
        . /opt/ros/melodic/setup.bash
    fi
fi

#------------------------------------------------------------------------------#
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64

# athena certificates path
export CERT_PATH=$HOME/.local/share/certificates

# Airflow
export CLUSTER_DEPLOYMENTS_HOME="$HOME/workspace/cluster-deployments"
export RECOMPUTE_FLOW_HOME="$HOME/workspace/recompute-flow"
export INCUBATOR_AIRFLOW_HOME="$HOME/workspace/incubator-airflow"
export AIRFLOW_HOME="$HOME/workspace/recompute-flow/airflow-home"
export AIRFLOW_HOST="localhost"
export AIRFLOW_PORT="8080"
export SPARK_SUBMIT_COMMAND="$SPARK_HOME/bin/spark-submit"

export SLUGIFY_USES_TEXT_UNIDECODE=yes
export AIRFLOW__CORE__PARALLELISM=10

export KUBERNETES_CONTEXT=kubernetes-dol-master@abstatt
export KUBERNETES_NAMESPACE=development-$USER
export DOCKER_REGISTRY=cmtcdeu58434236.rd.corpintra.net:32455
export HADOOP_NAME_NODE=http://cmtcdeu53965055.rd.corpintra.net:50070
export HADOOP_HDFS_ENDPOINT=hdfs://nameservice1
export HADOOP_USER_NAME=airflow

token() {
    kubectl -n kube-system describe secret admin-user-token-d57m4 |\
    awk '/token:/ {print $2;}' |\
    xclip -selection c
}

# Recompute flow
export PYTHONPATH=$PYTHONPATH:$RECOMPUTE_FLOW_HOME/airflow-home/dags:$RECOMPUTE_FLOW_HOME/micro_pipeline:$RECOMPUTE_FLOW_HOME/web-ui/server

# Postgres debug port
export POSTGRES_PORT=2345

#------------------------------------------------------------------------------#
# Kubernetes setup
if command -v kubectl &> /dev/null
then
    if [[ -n "${ZSH_NAME}" ]]; then
        source <(kubectl completion zsh)
    elif [[ -n "${BASH}" ]]; then
        source <(kubectl completion bash)
    fi
fi

# npm
NPM_VERSION='v13.14.0'
NPM_DISTRO='linux-x64'
export PATH="/usr/local/lib/nodejs/node-${NPM_VERSION}-${NPM_DISTRO}/bin:${PATH}"

# SGpp
export SGPP_HOME=$HOME/workspace/SGpp_ff
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SGPP_HOME/lib/sgpp
export PYTHONPATH=$PYTHONPATH:$SGPP_HOME/lib

# Player 2.0
export RECAPP_INT_HOME=$HOME/workspace/recapp_int
export RECAPP_HOME=$RECAPP_INT_HOME/recompute
export RECAPP_INT_RELEASE_DIR=$RECAPP_INT_HOME/install
export RECAPP_RELEASE_DIR=$RECAPP_INT_HOME/install/recapp

export PATH=$PATH:$RECAPP_RELEASE_DIR/bin:$RECAPP_RELEASE_DIR/bin/dol
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$RECAPP_RELEASE_DIR/lib
export PYTHONPATH=$PYTHONPATH:$RECAPP_RELEASE_DIR/lib/python2.7/dist-packages:$RECAPP_RELEASE_DIR/lib:$RECAPP_RELEASE_DIR/bin/bytesoup_inspector

# Azure DevOps
# Run cat BOSCH-CA-DE_pem.cer /opt/az/lib/python3.6/site-packages/certifi/cacert.pem > azure-bosch-cert.pem
export REQUESTS_CA_BUNDLE="${HOME}/.local/share/certificates/bosch/azure-bosch-cert.pem"

# Virtual environments for python
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
# Make sure that the debian package virtualenvwrapper is installed for
# the following to work. If errors occur, install it via "pip3 install
# virtualenvwrapper"
if [[ -f "/usr/share/virtualenvwrapper/virtualenvwrapper.sh" ]]; then
    source "/usr/share/virtualenvwrapper/virtualenvwrapper.sh"

    # pip bash completion start
    _pip_completion()
    {
        COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                                 COMP_CWORD=$COMP_CWORD \
                                 PIP_AUTO_COMPLETE=1 $1 ) )
    }
    complete -o default -F _pip_completion pip
    # pip bash completion end
fi

# >>> conda initialize >>>
if [[ -f "$HOME/anaconda3/bin/conda" ]]; then
    # !! Contents within this block are managed by 'conda init' !!
    __conda_setup="$('$HOME/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "$HOME/anaconda3/etc/profile.d/conda.sh" ]; then
            . "$HOME/anaconda3/etc/profile.d/conda.sh"
        else
            export PATH="$HOME/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
    conda deactivate
fi
# <<< conda initialize <<<


# make aliases available in eshell
alias | sed 's/^alias //' | sed -E "s/^([^=]+)='(.+?)'$/\1=\2/" | sed "s/'\\\\''/'/g" | sed "s/'\\\\$/'/;" | sed -E 's/^([^=]+)=(.+)$/alias \1 \2/' > ~/.emacs.d/eshell/alias
