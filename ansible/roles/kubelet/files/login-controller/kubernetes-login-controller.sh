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

if [ -f /etc/kubernetes/login-controller.conf ]; then
  source /etc/kubernetes/login-controller.conf
fi

enable_node () {
  if [ "$( get_status )" -eq 0 ] ; then
    echo "enabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS uncordon $KUBELET_NAME
  else
    echo "$KUBELET_NAME is already enabled"
  fi
}

disable_node () {
  if [ "$( get_status )" -eq 1 ] ; then
    echo "disabling node $KUBELET_NAME"
    kubectl $KUBECTL_PARAMS drain $KUBELET_NAME --force
  else
    echo "$KUBELET_NAME is already disabled"
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


while true; do
  user_count=$(who -u | wc -l)
  echo "$user_count user(s) are logged in."
  if [ "$user_count" -gt 0 ] ; then
    disable_node
  else
    enable_node
  fi

  sleep 10
done


