BASEDIR=info/hircus/kanren
TITLE="Mini Kanren"

bin: src/${BASEDIR}/*.scala
	$(shell [ -d bin ] && touch -m bin || mkdir -p bin)
	scalac -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

check:
	./check.sh

api: src/${BASEDIR}/*.scala
	$(shell [ -d api ] && touch -m api || mkdir -p api)
	scaladoc -doctitle ${TITLE} -windowtitle ${TITLE} -d api \
	src/${BASEDIR}/*.scala

clean:
	-rm -rf api bin

publish: api
	make -C docs
	./publish.sh
