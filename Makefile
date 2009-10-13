BASEDIR=info/hircus/kanren
all: src/${BASEDIR}/MiniKanren.scala
	scalac -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

check:
	cd bin
	scala info.hircus.kanren.tests.SubstSpecification
	scala info.hircus.kanren.tests.UnifySpecification

clean:
	-rm -rf bin/*
