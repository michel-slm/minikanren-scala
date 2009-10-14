#!/bin/sh
cd bin
for tst in info.hircus.kanren.tests.{Subst,Unify,Run,Branching,Math}Specification; do
  scala $tst
done
cd ..
