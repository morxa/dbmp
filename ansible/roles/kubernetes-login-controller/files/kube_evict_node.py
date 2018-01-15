#! /usr/bin/python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
# Copyright © 2018 Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
# Distributed under terms of the MIT license.

"""
Delete all pods running on a given host with given label.
"""

import argparse
import kubernetes
import socket

def main():
    arg_parser = argparse.ArgumentParser(
        description='Delete all pods running on a given host with given label.')
    arg_parser.add_argument('--host', default=socket.gethostname(),
                            help='only delete pods running on this host')
    arg_parser.add_argument('-l', '--label', type=str, default='evictable',
                           help='only delete pods with this label')
    arg_parser.add_argument('-v', '--label-value', type=str, default='true',
                           help='only delete pods with this label value')
    arg_parser.add_argument('-c', '--config', type=str,
                            default='/etc/kubernetes/kubeconfig',
                            help='path to the kubeconfig file')
    args = arg_parser.parse_args()
    kubernetes.config.load_kube_config(config_file=args.config)
    client = kubernetes.client.CoreV1Api()
    pods = client.list_pod_for_all_namespaces().items
    assert args.label != '', 'Cannot use "" as label selector.'
    for p in pods:
        if p.spec.node_name == args.host and \
           p.metadata.labels.get(args.label, '') == args.label_value:
            print('Deleting pod {}.'.format(p.metadata.name))
            client.delete_namespaced_pod(p.metadata.name,
                                         p.metadata.namespace,
                                         kubernetes.client.V1DeleteOptions())

if __name__ == '__main__':
    main()
