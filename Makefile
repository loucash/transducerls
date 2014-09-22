PROJECT := transducerls

ERL := erl
EPATH = -pa ebin -pz deps/*/ebin
TEST_EPATH = -pz deps/*/ebin -I deps/proper/include -pa ebin -pa test
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS= -Wno_undefined_callbacks --fullpath

.PHONY: all build_plt compile console deps clean depclean distclean dialyze test test-console

all: deps compile

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile-fast:
	@./rebar skip_deps=true compile

compile:
	@./rebar compile

console:
	$(ERL) -sname $(PROJECT) $(EPATH)

deps:
	@./rebar get-deps

clean:
	@./rebar skip_deps=true clean

depclean:
	@./rebar clean

distclean:
	@./rebar clean delete-deps
	@rm -rf logs

dialyze:
	@dialyzer $(DIALYZER_OPTS) -r ebin

test-compile:
	@./rebar get-deps compile

test: test-compile
	@./rebar skip_deps=true ct verbose=1

test-console: test-compile
	@erlc $(TEST_EPATH) -o test test/*.erl
	$(ERL) -sname $(PROJECT)_test  $(TEST_EPATH)
