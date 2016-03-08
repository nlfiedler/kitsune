#
# Makefile provided mostly for convenience purposes.
#
.PHONY: deps precompile test release

VERSION = _rel/kitsune/Version

deps:
	rebar get-deps
	cd deps/lager && $(MAKE)
	rebar prepare-deps

precompile:
	@(test -d deps || $(MAKE) deps)

test:
	rebar compile
	rebar ct

release: precompile
	rebar clean
	rebar compile
	relx
	@echo 'Build Date:' `date -R` > $(VERSION)
	@echo 'HEAD Commit:' `git log --max-count=1 --pretty='%h'` >> $(VERSION)
