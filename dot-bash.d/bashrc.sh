ossystem=$(uname -s)
unamen=$(uname -n)

if [[ ${ossystem} == [Dd]arwin ]] ; then
    # OS main revision number
    osver=$(uname -r)
    osver=${osver%%\.*}
    # macOS 10.x
    osdigit=$(( ${osver} - 4 ))
    for (( i=9 ; i<=19 ; i++ )) ; do
        if [[ ${osver} -eq ${i} ]] ; then
	    [[ -f ${HOME}/.bashrc.10.${osdigit} ]] && source ${HOME}/.bashrc.10.${osdigit}
	fi
    done
    # macOS 11
    osdigit=$(( $osver - 15 ))
    if [[ ${osver} -eq 20 ]] ; then
	[[ -f ${HOME}/.bashrc.11.${osdigit} ]] && source ${HOME}/.bashrc.11.${osdigit}
    fi
    # macOS >11
    osdigit=$(( $osver - 9 ))
    if [[ ${osver} -gt 20 ]] ; then
	[[ -f ${HOME}/.bashrc.${osdigit} ]] && source ${HOME}/.bashrc.${osdigit}
    fi
elif [[ ${ossystem} == [Ll]inux ]] ; then
    if [[ ${unamen} == master01 || ${unamen} == *.cluster.local ]] ; then
	[[ -f ${HOME}/.bashrc.biocomp ]] && source ${HOME}/.bashrc.biocomp
    elif [[ ${unamen} == gadi* ]] ; then
	[[ -f ${HOME}/.bashrc.gadi ]] && source ${HOME}/.bashrc.gadi
    elif [[ $(hostname --domain) == *explor ]] ; then
	[[ -f ${HOME}/.bashrc.explor ]] && source ${HOME}/.bashrc.explor
    elif [[ ${unamen} == login* ]] ; then
	[[ -f ${HOME}/.bashrc.curta ]] && source ${HOME}/.bashrc.curta
    fi
elif [[ ${ossystem} == [Cc]ygwin* ]] ; then
    [[ -f ${HOME}/.bashrc.cygwin ]] && source ${HOME}/.bashrc.cygwin
fi
