#! /bin/bash
#
# login-controller.sh
# Copyright (C) 2016 Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
# Distributed under terms of the MIT license.
#

set -euo pipefail

KUBECTL_PARAMS=${KUBECTL_PARAMS:-""}
KUBELET_NAME=${KUBELET_NAME:-$(hostname -s)}
ENABLE_LOGIN_CONTROLLER=1

if [ -f /etc/kubernetes/login-controller.conf ]; then
  source /etc/kubernetes/login-controller.conf
fi

if [ "$ENABLE_LOGIN_CONTROLLER" -ne 1 ] ; then
  exit 0
fi

enable_node () {
  if [ "$( get_status )" -eq 0 ] ; then
    echo "enabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS uncordon $KUBELET_NAME
  fi
}

disable_node () {
  if [ "$( get_status )" -eq 1 ] ; then
    echo "disabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS drain $KUBELET_NAME --force
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

if [ -z ${PAM_TYPE+0} ] || [ -z ${PAM_USER+0} ] ; then
  echo "PAM_TYPE or PAM_USER is not set or empty."
  exit 1
fi

if [ "$PAM_TYPE" = "close_session" ] ; then
  user_count=$( who -u | grep -c -v "^root" || true)
  if [ "$user_count" -eq 0 ] ; then
    enable_node
  fi
elif [ "$PAM_TYPE" = "open_session" ] && [ "$PAM_USER" != "root" ] ; then
  disable_node
fi

