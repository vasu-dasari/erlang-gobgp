.PHONY: compile update clean deep-clean

DOCKER=$(shell which docker)
REBAR=rebar3

ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

DOCKER_IMAGE := erlang_gobgp
DOCKER_RUN := $(DOCKER) run -d -it --name $(DOCKER_IMAGE) \
		-v $(shell pwd):$(shell pwd) -w $(shell pwd)
DOCKER_EXEC := $(DOCKER) exec -it $(DOCKER_IMAGE) bash -c 
DOCKER_EXEC_ARGS=cd $(shell pwd) &&
DOCKER_REBAR := $(DOCKER_EXEC_ARGS) $(REBAR)

# ERL_FLAGS=" -args_file config/vm.args -config config/sys.config" rebar3 shell
SHELL_ARGS := ERL_FLAGS=\" -args_file config/vm.args -config config/sys.config\" $(REBAR) shell

all: run

container:
ifneq ($(shell $(DOCKER) ps -f name=$(DOCKER_IMAGE) | grep Up > /dev/null; echo $$?),0)
	 @$(shell $(DOCKER) stop $(DOCKER_IMAGE) &> /dev/null)
	 @$(shell $(DOCKER) rm $(DOCKER_IMAGE) &> /dev/null)
	 @$(DOCKER_RUN) vdasari/erlango /bin/bash
endif

container-clean:
	@$(DOCKER) stop $(DOCKER_IMAGE)
	@$(DOCKER) rm $(DOCKER_IMAGE)

compile: container
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) compile"

update: container
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) update"
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) upgrade"
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) compile"

clean: container
	@$(DOCKER_EXEC) "$(DOCKER_REBAR) clean"

deep-clean: clean
	@rm -rf _build rebar.lock

shell: container
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) bash"

run: compile
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) $(SHELL_ARGS)"

.SILENT: