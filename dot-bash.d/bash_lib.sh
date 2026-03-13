# after pathmunge of H Schwartz: https://github.com/hrs/dotfiles/
prepath() {
    # return prepended arbitrary PATH variable
    #   export MANPATH=$(${MANPATH} /path/to/add)
    if [[ $# -gt 1 ]] ; then
        if ! echo ${1} | grep -E -q "(^|:)${2}($|:)" ; then
            # /path/to/add not yet in PATH; /path/to/add must exist
            [[ -d ${2} ]] && echo ${2}:${1} || echo ${1}
        else
            # /path/to/add already in PATH
            echo ${1}
        fi
    else
        # PATH does not exist yet; /path/to/add must exist
        [[ -d ${1} ]] && echo ${1} || echo ""
    fi
}

apppath() {
    # return appended arbitrary PATH variable
    #   export MANPATH=$(${MANPATH} /path/to/add)
    if [[ $# -gt 1 ]] ; then
        if ! echo ${1} | grep -E -q "(^|:)${2}($|:)" ; then
            # /path/to/add not yet in PATH; /path/to/add must exist
            [[ -d ${2} ]] && echo ${1}:${2} || echo ${1}
        else
            # /path/to/add already in PATH
            echo ${1}
        fi
    else
        # PATH does not exist yet; /path/to/add must exist
        [[ -d ${1} ]] && echo ${1} || echo ""
    fi
}
