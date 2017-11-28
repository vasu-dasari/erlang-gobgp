.PHONY: compile update clean deep-clean

DOCKER=$(shell which docker)
REBAR=rebar3

ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

DOCKER_NAME := erlang_gobgp
DOCKER_EXEC := $(DOCKER) exec -it $(DOCKER_NAME) bash -c
DOCKER_EXEC_ARGS=cd $(shell pwd) &&
DOCKER_REBAR := $(DOCKER_EXEC_ARGS) $(REBAR)

# ERL_FLAGS=" -args_file config/vm.args -config config/sys.config" rebar3 shell
SHELL_ARGS := ERL_FLAGS=\" -args_file config/vm.args -config config/sys.config\" $(REBAR) shell

all: compile

container:
	@make up erlang_gobgp

container-clean:
	@make down erlang_gobgp

compile:
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) compile"

update:
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) update"
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) upgrade"
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) compile"

clean:
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) clean"

deep-clean: clean
	@rm -rf _build rebar.lock

shell:
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) bash"

run:
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) $(SHELL_ARGS)"

%:
	@:

include apps/examples/Makefile
.SILENT:
