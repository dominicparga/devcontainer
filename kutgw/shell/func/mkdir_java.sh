mkdir_java() {
    return # TODO

    # |-------|
    # | utils |
    # |-------|

    __print_usage() {
        # params
        local _prg_call; _prg_call=${3}
        local _err_code; _err_code=${1}
        local _err_msg;  _err_msg=${2}


        # print error message if existing
        if [[ ! -z ${_err_msg} ]]
        then
            printf "${_err_msg}\n\n"
        fi
        # print usage info
        printf "Description:\n"
        printf "    Create a java project directory tree, looking like this:\n"
        printf "        <root_dir>\n"
        printf "        ├-- lib\n"
        printf "        ├-- res\n"
        printf "        └-- src\n"
        printf "Usage:\n"
        printf "    ${_prg_call} [options] <root_dir>\n"
        printf "options:\n"
        printf "    -h --help\n"
        printf "        Show this help message\n"
        printf "arguments:\n"
        printf "    <root_dir>\n"
        printf "        The project directory\n"
    }



    # |--------------|
    # | store params |
    # |--------------|

    local _prg_call=${0}
    local _root_dir=${1}
    # TODO cmdline parser
    # TODO -h|--help



    # |-----------------|
    # | checking params |
    # |-----------------|

    if [[ -e ${_root_dir} ]]
    then
        local _err_code=1
        local _err_msg="Given <root_dir> '${_root_dir}' does already exist."
    fi

    if [[ ! -z ${_err_code} ]]
    then
        __print_usage ${_err_code} "${_err_msg}" ${_prg_call}
        return ${_err_code}
    fi



    # |---------|
    # | execute |
    # |---------|

    mkdir -p ${_root_dir}
    mkdir ${_root_dir}/lib
    mkdir ${_root_dir}/res
    mkdir ${_root_dir}/src

    printf "Java directory tree\n"
    printf "    ${_root_dir}\n"
    printf "    ├-- lib\n"
    printf "    ├-- res\n"
    printf "    └-- src\n"
    printf "created.\n"



    # |---------|
    # | cleanup |
    # |---------|

    unset -f __print_usage
}
