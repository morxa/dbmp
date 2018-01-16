#! /bin/bash
#
# macroff_gen.bash
# Copyright (C) 2018 Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
# Distributed under terms of the MIT license.
#


set -eou pipefail

TIMEOUT=3600

print_usage () {
  echo "$0 DOMAIN PROBLEM..."
}

if [[ $# -eq 1 ]] && [[ "$1" = "-h" ]] ; then
  print_usage
  exit 0
fi

if [ $# -lt 2 ] ; then
  print_usage
  exit 1
fi

domain=$1
problems=${@:2}

macro_file="macros.pddl"

for problem in $problems ; do
  sed -i '/^TIME/d' $problem
  if [ -e macros.pddl ] ; then
    timeout $TIMEOUT macroff -q $macro_file -o $domain -f $problem
  else
    timeout $TIMEOUT macroff -m P -r $macro_file -o $domain -f $problem
  fi
done

