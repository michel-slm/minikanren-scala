#!/bin/sh
if [ -z "$1" ]; then
  HOST=salimma@fedorapeople.org
else
  HOST=$1
fi

if [ -z "$2" ]; then
  PUBDIR=public_html/presentations/mk
else
  PUBDIR=$2
fi

tar cf - presentation.html ui | ssh $HOST "mkdir -p $PUBDIR && \
	cd $PUBDIR && tar xf -"
