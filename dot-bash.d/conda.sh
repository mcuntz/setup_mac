unamen=$(uname -n)

myconda="${HOME}/miniforge3"
if [[ ${unamen} == master01 || ${unamen} == *.cluster.local ]] ; then
    # biocomp
    if [[ ${unamen} == gpu02* ]] ; then
	[[ -d ${HOME}/amdforge3 ]] && myconda="${HOME}/amdforge3"
    else
	[[ -d ${HOME}/intelforge3 ]] && myconda="${HOME}/intelforge3"
    fi
fi
# # miniconda from pyenv
# myconda="${HOME}/.pyenv/versions/miniconda3-4.7.12"
if [[ -f ${myconda}/bin/conda ]] ; then
    # initialize conda
    __conda_setup="$(${myconda}/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
    if [[ $? -eq 0 ]] ; then
	eval "$__conda_setup"
    else
	if [[ -f "${myconda}/etc/profile.d/conda.sh" ]] ; then
            source "${myconda}/etc/profile.d/conda.sh"
	else
            export PATH="${myconda}/bin:${PATH}"
	fi
    fi
    unset __conda_setup
fi
