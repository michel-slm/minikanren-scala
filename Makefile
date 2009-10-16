BASEDIR=info/hircus/kanren
bin: src/${BASEDIR}/*.scala
	$(shell [ -d bin ] && touch -m bin || mkdir -p bin)
	scalac -d bin src/${BASEDIR}/*.scala src/${BASEDIR}/tests/*.scala

check:
	./check.sh

api: src/${BASEDIR}/*.scala
	$(shell [ -d api ] && touch -m api || mkdir -p api)
	scaladoc -d api src/${BASEDIR}/*.scala

clean:
	-rm -rf bin/*

publish: api
	make -C docs
	./publish.sh
