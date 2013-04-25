BASEDIR=info/hircus/kanren
TITLE="Mini Kanren"
LIBS=libs
CLASSPATH=$(LIBS)/scalacheck_2.10-1.10.2-SNAPSHOT.jar:$(LIBS)/clojure-1.5.1-slim.jar
SCALAFLAGS=-feature -deprecation

all: bin bin/${BASEDIR}/examples

bin: src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala
	$(shell [ -d bin ] && touch -m bin || mkdir -p bin)
	scalac $(SCALAFLAGS) -cp $(CLASSPATH) -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

bin/${BASEDIR}/examples: bin src/${BASEDIR}/examples/*.scala
	scalac $(SCALAFLAGS) -cp $(CLASSPATH):bin -d bin src/${BASEDIR}/examples/*.scala

check: 
	./check.sh

api: src/${BASEDIR}/*.scala src/${BASEDIR}/examples/*.scala
	$(shell [ -d api ] && touch -m api || mkdir -p api)
	scaladoc -doctitle ${TITLE} -windowtitle ${TITLE} -d api \
	src/${BASEDIR}/*.scala src/${BASEDIR}/examples/*.scala

clean:
	-rm -rf api bin

publish: api
	make -C docs
	./publish.sh
