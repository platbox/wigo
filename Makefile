APP=wigo
REBAR ?= $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: compile clean distclean start test

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean:
	rm -rfv _build log

start:
	exec erl +K true -pz _checkouts/*/ebin -pz _build/default/lib/*/ebin

test:
	$(REBAR) eunit ct
