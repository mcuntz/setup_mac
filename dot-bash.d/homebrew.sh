if [[ -d /opt/homebrew ]] ; then
    # eval $(/opt/homebrew/bin/brew shellenv)
    # or by hand to append rather than prepand path
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
elif [[ -e /usr/local/bin/brew ]] ; then
    export HOMEBREW_PREFIX="/usr/local";
    export HOMEBREW_CELLAR="/usr/local/Cellar";
    export HOMEBREW_REPOSITORY="/usr/local";
fi

if [[ -n ${HOMEBREW_PREFIX} ]] ; then
    export PATH=${HOMEBREW_PREFIX}/bin:${HOMEBREW_PREFIX}/sbin:${PATH}
fi
