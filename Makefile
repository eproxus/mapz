.PHONY: test

all: compile test

compile:
	@rebar compile

test-deps:
	@rebar get-deps -C test.config

test-compile: test-deps
	@rebar compile -C test.config

test-fast:
	@rebar eunit -C test.config

test: test-compile test-fast
