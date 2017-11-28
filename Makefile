.PHONY: compile update clean deep-clean

DOCKER=$(shell which docker)
REBAR=rebar3
DOCKER_COMPOSE = docker/docker-compose.yml

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

build:
	docker-compose -f $(DOCKER_COMPOSE) build

up:
ifeq ($(MAKECMDGOALS),up)
	docker-compose -f $(DOCKER_COMPOSE) up -d
else
	docker-compose -f $(DOCKER_COMPOSE) up -d $(filter-out $@,$(MAKECMDGOALS))
endif

down:
ifeq ($(MAKECMDGOALS),down)
	docker-compose -f $(DOCKER_COMPOSE) down
else
	docker-compose -f $(DOCKER_COMPOSE) stop $(filter-out $@,$(MAKECMDGOALS))
	docker-compose -f $(DOCKER_COMPOSE) rm -f $(filter-out $@,$(MAKECMDGOALS))
endif

start:
ifeq ($(MAKECMDGOALS),start)
	docker-compose -f $(DOCKER_COMPOSE) start
else
	docker-compose -f $(DOCKER_COMPOSE) start $(filter-out $@,$(MAKECMDGOALS))
endif

stop:
ifeq ($(MAKECMDGOALS),stop)
	docker-compose -f $(DOCKER_COMPOSE) stop
else
	docker-compose -f $(DOCKER_COMPOSE) stop $(filter-out $@,$(MAKECMDGOALS))
endif

connect:
	$(DOCKER) exec -it $(filter-out $@,$(MAKECMDGOALS))  bash -c "cd $(shell pwd)/docker && bash"

logs:
	$(DOCKER) logs $(filter-out $@,$(MAKECMDGOALS)) --follow

shell:
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) bash"

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

run:
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) $(SHELL_ARGS)"

%:
	@:

.SILENT:
