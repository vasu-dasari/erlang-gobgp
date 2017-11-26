run: container
	@$(DOCKER_EXEC) "$(DOCKER_EXEC_ARGS) $(SHELL_ARGS)"

DOCKERPATH = docker

DOCKER_COMPOSE = apps/examples/docker/docker-compose.yml

.PHONY: build
build:
	docker-compose -f $(DOCKER_COMPOSE) build

.PHONY: up
up:
ifeq ($(MAKECMDGOALS),up)
	docker-compose -f $(DOCKER_COMPOSE) up -d
else
	docker-compose -f $(DOCKER_COMPOSE) up -d $(filter-out $@,$(MAKECMDGOALS))
endif

.PHONY: down
down:
ifeq ($(MAKECMDGOALS),down)
	docker-compose -f $(DOCKER_COMPOSE) down
else
	docker-compose -f $(DOCKER_COMPOSE) stop $(filter-out $@,$(MAKECMDGOALS))
	docker-compose -f $(DOCKER_COMPOSE) rm -f $(filter-out $@,$(MAKECMDGOALS))
endif

connect:
	$(DOCKER) exec -it $(filter-out $@,$(MAKECMDGOALS))  bash -c "cd $(shell pwd)/examples/docker && bash"

logs:
	$(DOCKER) logs $(filter-out $@,$(MAKECMDGOALS)) --follow
