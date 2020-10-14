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

# greet

#------------------------------------------------------------------------------#

# ----------------------------------------------------
# personal

# use emacs keybindings
if [[ -n "${ZSH_NAME}" ]]; then
    bindkey -e

    autoload -U bashcompinit
    bashcompinit
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

# expand path to include local bin directory
PATH=/usr/local/cmake/3.15.3/bin:$HOME/opt/bin:$HOME/.local/bin:$PATH

# moving files to trash from command line
alias "trash"="gvfs-trash"

# # create aliases for emacs eshell
# alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;" >~/.emacs.d/eshell/alias

alias dfs="hdfs dfs"

EMACS="emacsclient -c -a emacs %f"

#------------------------------------------------------------------------------#
# ros setup

if [[ -n "${ZSH_NAME}" ]]; then
    # melodic is only for Ubuntu 18.04
    if [[ -f /opt/ros/melodic/setup.zsh ]]; then
        . /opt/ros/melodic/setup.zsh
    fi
elif [[ -n "${BASH}" ]]; then
    # melodic is only for Ubuntu 18.04
    if [[ -f /opt/ros/melodic/setup.zsh ]]; then
        . /opt/ros/melodic/setup.bash
    fi
fi

#------------------------------------------------------------------------------#
# setup hadoop, and gradle
export HADOOP_HOME=$HOME/opt/hadoop-3.1.0
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/jre

export PATH=$SPARK_HOME/bin:$HADOOP_HOME/bin:$HOME/opt/gradle/gradle-4.7/bin:$PATH

# add kerberbos to LD_LIBRARY_PATH
export KERBEROS_HOME=$HOME/opt/krb5-1.16
export LIBHDFS3_ROOT=$HOME/opt/attic-c-hdfs-client-apache-rpc-9
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$KERBEROS_HOME/lib:$LIBHDFS3_ROOT/lib

# athena certificates path
export CERT_PATH=$HOME/.local/share/certificates

# athena -> with athena_dol
export ATHENA_ROOT=$HOME/workspace/athena
export WORKSPACE=$ATHENA_ROOT
export HOST_ARTIFACTS_CACHE=$HOME/artifacts
# export DOL_HOST="http://172.17.0.2:5000"

# required by lidar
export C2C_CAR_ID=v222-5218 # lisa hks22
export C2C_HW_VERSION=3.1

# export cuda paths
export CUDA_ROOT=/usr/local/cuda
export CUDA_INC_DIR=$CUDA_ROOT/include
source /etc/profile.d/cuda-10-0.sh

# Airflow
export CLUSTER_DEPLOYMENTS_HOME="$HOME/workspace/cluster-deployments"
export RECOMPUTE_FLOW_HOME="$HOME/workspace/recompute-flow"
export INCUBATOR_AIRFLOW_HOME="$HOME/workspace/incubator-airflow"
export AIRFLOW_HOME="$HOME/workspace/recompute-flow/airflow-home"
export AIRFLOW_HOST="localhost"
export AIRFLOW_PORT="8080"
export SPARK_SUBMIT_COMMAND="$SPARK_HOME/bin/spark-submit"
export YARN_CONF_DIR="$HADOOP_HOME/etc/hadoop"

export SLUGIFY_USES_TEXT_UNIDECODE=yes

export KUBERNETES_CONTEXT=kubernetes-dol-master@abstatt
export KUBERNETES_CONTEXT_SUNNYVALE=dol_master@sunnyvale
export KUBERNETES_NAMESPACE=development-$USER
export KUBERNETES_NAMESPACE_SUNNYVALE=sunnyvale-mock
export DOCKER_REGISTRY=cmtcdeu58434236.rd.corpintra.net:32455
export DOCKER_REGISTRY_SUNNYVALE=cmtcdeu53965049.rd.corpintra.net:31753
export HADOOP_NAME_NODE=http://cmtcdeu53965055.rd.corpintra.net:50070
export HADOOP_HDFS_ENDPOINT=hdfs://nameservice1
export HADOOP_USER_NAME=airflow

export ADSTATS_HOST="http://s624duadwebapps.us624.corpintra.net:8080/"

token() {
    kubectl -n kube-system describe secret admin-user-token-d57m4 |\
    awk '/token:/ {print $2;}' |\
    xclip -selection c
}

token_sv() {
    kubectl -n kube-system describe secret $(kubectl -n kube-system get secret --context dol_master@sunnyvale | grep admin-user | awk '{print $1}') --context dol_master@sunnyvale |\
    awk '/token:/ {print $2;}' |\
    xclip -selection c
}


# Recompute flow
export PYTHONPATH=$PYTHONPATH:$RECOMPUTE_FLOW_HOME/airflow-home/dags

# Postgres debug port
export POSTGRES_PORT=2345

#------------------------------------------------------------------------------#
# Kubernetes setup

if [[ -n "${ZSH_NAME}" ]]; then
    source <(kubectl completion zsh)
elif [[ -n "${BASH}" ]]; then
    source <(kubectl completion bash)
fi

# Lidar semantic labelingcod
export EXPORTER_BASE_OUTPATH=/tmp/lidar
export VEHICLE_IDENTIFIER=WDD2221621Z003456

# SGpp
export SGPP_HOME=$HOME/workspace/SGpp_ff
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SGPP_HOME/lib/sgpp
export PYTHONPATH=$PYTHONPATH:$SGPP_HOME/lib

# Player 2.0
export RECAPP_RELEASE_DIR=/opt/recapp/0.11a.0jtv1/ubuntu1804
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$RECAPP_RELEASE_DIR/lib
export PYTHONPATH=$PYTHONPATH:$RECAPP_RELEASE_DIR/lib/python2.7/dist-packages:$RECAPP_RELEASE_DIR/lib:$RECAPP_RELEASE_DIR/bin/bytesoup_inspector

# Virtual environments for python
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
# Make sure that the debian package virtualenvwrapper is installed for
# the following to work. If errors occur, install it via "pip3 install
# virtualenvwrapper"
source /usr/share/virtualenvwrapper/virtualenvwrapper.sh

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
# pip bash completion end


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/lhome/franzef/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/lhome/franzef/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/lhome/franzef/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/lhome/franzef/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
conda deactivate
# <<< conda initialize <<<

# Created by `userpath` on 2020-10-09 10:40:30
export PATH="$PATH:/lhome/franzef/.local/bin"

# Autocompletion for pipx
eval "$(register-python-argcomplete pipx)"
