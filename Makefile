install:
	mkdir -p `guile-config info sitedir`
	cp -R gson.scm gson `guile-config info sitedir`

uninstall:
	rm -R `guile-config info sitedir`/gson*

test:
	git submodule update --init --recursive
	make -C JSONTestSuite test
