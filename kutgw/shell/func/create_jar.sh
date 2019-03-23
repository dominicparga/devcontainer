create_jar() {
    return

    # TODO parameters: root-folder, jar-filename
    # TODO build java seperate from this function

    # |-------|
    # | utils |
    # |-------|

    __print_usage() {
        # params
        local _prg_call; _prg_call=${0}
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
        printf "    ${_prg_call} [options] root=<root_dir>\n"
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

    local _root_dir=${1#*=} # TODO cmdline parser



    # |-----------------|
    # | checking params |
    # |-----------------|

    if [[ ! -e ${_root_dir} ]]
    then
        local _err_code=1
        local _err_msg="Given <root_dir> '${_root_dir}' does not exist."
    elif [[ ! -d ${_root_dir} ]]
    then
        local _err_code=1
        local _err_msg="Given <root_dir> '${_root_dir}' is not a directory."
    fi

    if [[ ! -z _err_code ]]
    then
        __print_usage ${_err_code} "${_err_msg}"
        return ${_err_code}
    fi



    # |---------|
    # | execute |
    # |---------|

    echo ${_root_dir} # TODO



    # folder structure before running this script
    # root
    # ├-- src
    # └-- lib

    # folder structure after running this script
    # root
    # ├-- src
    # ├-- lib
    # └-- build
    #     ├-- META-INF
    #     |   └-- MANIFEST.MF
    #     ├-- bin
    #     |   └-- *.class
    #     └-- lib


    # reset build
    mkdir build
    rm -r build/*

    # create MANIFEST.MF
    mkdir build/META-INF
    printf "Manifest-Version: 1.0
    Main-Class: core.NicosSonnensytem
    Class-path: lib/core.jar" > build/META-INF/MANIFEST.MF
    echo "\n" >> build/META-INF/MANIFEST.MF

    # build lib
    mkdir build/lib
    cd build/lib
    jar -xf ../../lib/core.jar
    cd ../..

    # create *.class files
    mkdir build/bin
    /Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/bin/javac \
    -source 8 \
    -d build/bin \
    -sourcepath src \
    -cp lib/core.jar \
    src/core/NicosSonnensytem.java

    # create jar-file
    /Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/bin/jar \
    cfm \
    out.jar \
    build/META-INF/MANIFEST.MF \
    -C build/lib . \
    -C build/bin .



    # |---------|
    # | cleanup |
    # |---------|

    unset -f __print_usage
}
