if [[ -d ${HOME}/.pyenv ]] ; then
    export PYENV_ROOT="${HOME}/.pyenv"
    if [[ -d ${PYENV_ROOT}/libexec ]] ; then  # local install from github
        export PATH=${PYENV_ROOT}/libexec:${PATH}
    fi
    export PATH=${PYENV_ROOT}/shims:${PATH}
    if command -v pyenv 1>/dev/null 2>&1 ; then eval "$(pyenv init -)" ; fi
    if command -v pyenv virtualenv-init 1>/dev/null 2>&1 ; then eval "$(pyenv virtualenv-init -)" ; fi
fi
