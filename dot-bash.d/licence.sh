# mac address of main wifi adapter
ether=$(echo $(ifconfig en0 ether 2>/dev/null | grep -v option | sed -n -e 2p | sed -e '/ether/s/ *ether //'))

if [[ "${ether}" == "??:??:??:??:??:??" ]] ; then
    # MacBoock Air 2014
    export NAG_KUSARI_FILE=${HOME}/bin/licences/nag_license.6.1.node-lock.air
elif [[ "${ether}" == "??:??:??:??:??:??" ]] ; then
    # Special MacBoock Pro UFZ 2014
    export NAG_KUSARI_FILE=${HOME}/bin/licences/nag_license.6.1.node-lock.macbook
elif [[ "${ether}" == "??:??:??:??:??:??" ]] ; then
    # Special MacBoock Pro 13" INRA 2016
    # export NAG_KUSARI_FILE=${HOME}/bin/licences/nag_license.6.2.full.mcinra
    export NAG_KUSARI_FILE=${HOME}/bin/licences/nag_license.7.0.full.mcinra
    export INTEL_LICENSE_FILE=${HOME}/bin/licences/ifort_licence_2019.lic.mcinra
elif [[ "${ether}" == "??:??:??:??:??:??" ]] ; then
    # Special MacBoock Pro 16" INRAE 2020
    export NAG_KUSARI_FILE=${HOME}/bin/licences/nag.7.2.key
elif [[ "${ether}" == "??:??:??:??:??:??" ]] ; then
    # Special Mac Mini INRAE 2021
    export NAG_KUSARI_FILE=${HOME}/bin/licences/nag.7.2.key
fi
