BASEDIR=info/hircus/kanren
TITLE="Mini Kanren"

all: bin bin/${BASEDIR}/examples

bin: src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala
	$(shell [ -d bin ] && touch -m bin || mkdir -p bin)
	scalac -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

bin/${BASEDIR}/examples: bin src/${BASEDIR}/examples/*.scala
	scalac -d bin src/${BASEDIR}/examples/*.scala

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
