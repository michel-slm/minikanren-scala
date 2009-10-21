#!/bin/sh
if [ -z "$1" ]; then
  HOST=hircus@iceland.freeshell.org
else
  HOST=$1
fi

if [ -z "$2" ]; then
  PUBDIR=html/kanren/
else
  PUBDIR=$2
fi

rsync -avz --delete -e ssh api docs/{handouts,presentation}.html docs/ui \
  $HOST:$PUBDIR

