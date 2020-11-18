install:
	mkdir -p `guile-config info sitedir`
	cp -R gson.scm gson `guile-config info sitedir`

uninstall:
	rm -R `guile-config info sitedir`/gson*

test: install
	guile test.scm 2> /dev/null
