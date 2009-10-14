#!/bin/sh
cd bin
for tst in info.hircus.kanren.tests.{Subst,Unify,Run,Branching}Specification; do
  scala $tst
done
cd ..
