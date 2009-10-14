BASEDIR=info/hircus/kanren
all: src/${BASEDIR}/MiniKanren.scala
	mkdir -p bin
	scalac -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

check:
	./check.sh

apidoc:
	mkdir -p api
	scaladoc -d api src/${BASEDIR}/*.scala

clean:
	-rm -rf bin/*
