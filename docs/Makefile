all: presentation.html handouts.html

presentation.html: presentation.rst
	rst2s5 presentation.rst presentation.html

handouts.html: presentation.rst
	rst2html presentation.rst handouts.html

clean:
	-rm -rf presentation.html ui

