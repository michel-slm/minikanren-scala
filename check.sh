#!/bin/sh
LIBS=../libs
CLASSPATH=$LIBS/scalacheck_2.10-1.10.2-SNAPSHOT.jar:$LIBS/clojure-1.5.1-slim.jar
cd bin
for tst in info.hircus.kanren.tests.{Subst,Unify,Run,Branching,Math}Specification; do
  scala -cp $CLASSPATH:. $tst
done
cd ..
