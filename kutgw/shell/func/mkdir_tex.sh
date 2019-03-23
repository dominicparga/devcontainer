mkdir_tex() {
    # params
    local _root_dir=${1}


    # create a new LaTeX-directory tree
    local _parent="."

    if [ ! -z "${_root_dir}" ]
    then
        _parent="${_root_dir}"
    fi


    local _dirs=( \
        preamble \
        content \
        afterwords \
        graphics \
    )

    for dir in "${_dirs[@]}"
    do
        if [[ ! -e ${dir} ]]
        then
            printf "CREATE directory ${_parent}${dir}\n"
            mkdir "${_parent}${dir}"
            printf "\n"
        fi
    done


    local _files=(
        preamble/abbreviations.tex
        preamble/packages.tex
        content/abstract.tex
        content/intro.tex
        content/conclusion.tex
        afterwords/appendices.tex
        afterwords/bib.tex
        afterwords/biographies.tex
    )

    for f in "${_files[@]}"
    do
        printf "CREATE file ${_parent}${f}\n"
        touch "${_parent}${f}"
        printf "\n"
    done
}
