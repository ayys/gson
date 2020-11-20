install: install-scheme install-go

install-scheme:
	mkdir -p `guile-config info sitedir`
	cp -R gson.scm gson `guile-config info sitedir`

install-go: compile
	mkdir -p `guile-config info siteccachedir`
	cp -R build/* `guile-config info siteccachedir`

compile: gson.scm gson/loader.scm gson/dumper.scm gson/tokenizer.scm
	mkdir -p build
	mkdir -p build/gson
	guild compile gson.scm -o build/gson.go
	guild compile gson/loader.scm -o build/gson/loader.go
	guild compile gson/dumper.scm -o build/gson/dumper.go
	guild compile gson/tokenizer.scm -o build/gson/tokenizer.go

clean:
	rm -R build|| yes

uninstall:
	rm -R `guile-config info sitedir`/gson* || true
	rm -R `guile-config info siteccachedir`/gson* || true

test:
	git submodule update --init --recursive
	make -C JSONTestSuite test
