.PHONY: compile update clean deep-clean

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which docker_rebar)
ifeq ($(REBAR),)
REBAR=rebar3
endif

all: run

compile:
	@$(REBAR) compile

update:
	@$(REBAR) update
	@$(REBAR) upgrade
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deep-clean: clean
	rm -rf _build rebar.lock

run: compile
	ERL_FLAGS=" -args_file config/vm.args -config config/sys.config" $(REBAR) shell
