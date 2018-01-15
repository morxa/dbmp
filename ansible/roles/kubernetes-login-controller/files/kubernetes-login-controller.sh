#! /bin/bash
#
# login-controller.sh
# Copyright (C) 2016-2018 Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
# Distributed under terms of the MIT license.
#

set -euo pipefail

KUBECTL_PARAMS=${KUBECTL_PARAMS:-""}
KUBELET_NAME=${KUBELET_NAME:-$(hostname -s)}
ENABLE_LOGIN_CONTROLLER=1
KUBE_NAMESPACE=
# override $HOME which is set by PAM and points to the user's home
HOME=/

if [ -f /etc/kubernetes/login-controller.conf ]; then
  source /etc/kubernetes/login-controller.conf
fi

if [ "$ENABLE_LOGIN_CONTROLLER" -ne 1 ] ; then
  exit 0
fi

if [ "$KUBE_MASTER" = "" ] ; then
  echo "KUBE_MASTER not set. Stop."
  exit 1
fi

if [ "$( ping -W 1 -c 1 $KUBE_MASTER &>/dev/null || echo failed)" = "failed" ]
then
  echo "Cannot reach Kubernetes master $KUBE_MASTER. Stop."
  exit 0
fi

enable_node () {
  if [ "$( get_status )" -eq 0 ] ; then
    echo "enabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS uncordon $KUBELET_NAME
    # turn off swap because otherwise the containers will use it all the time,
    # cf. http://stackoverflow.com/questions/40553541/disable-swap-on-a-kubelet
    /usr/sbin/swapoff -a
  fi
}

disable_node () {
  if [ "$( get_status )" -eq 1 ] ; then
    echo "disabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS drain \
      ${KUBE_NAMESPACE:+"--namespace ${KUBE_NAMESPACE}" \
      $KUBELET_NAME
    /usr/sbin/swapon -a
  fi
}

# 0 if disabled, 1 if enabled
get_status () {
  status=$(kubectl $KUBECTL_PARAMS get node -o=json $KUBELET_NAME \
           | jq '.spec.unschedulable')
  if [ "$status" = "true" ] ; then
    echo 0
  else
    echo 1
  fi
}

count_users () {
  user_count=0
  user_count=$(who -u | grep -c -v "^root" || true)
  echo $user_count
}

if [ "$#" -eq 1 ] && [ "$1" = "reload" ] ; then
  user_count=$( count_users )
  if [ "$user_count" -eq 0 ] ; then
    enable_node
  else
    disable_node
  fi
  exit 0
fi

if [ -z ${PAM_TYPE+0} ] || [ -z ${PAM_USER+0} ] ; then
  echo "PAM_TYPE or PAM_USER is not set or empty."
  exit 1
fi

if [ "$PAM_TYPE" = "close_session" ] ; then
  user_count=$( count_users )
  if [ "$user_count" -eq 0 ] ; then
    enable_node
  fi
elif [ "$PAM_TYPE" = "open_session" ] && [ "$PAM_USER" != "root" ] ; then
  disable_node
fi

