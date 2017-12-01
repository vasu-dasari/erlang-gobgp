# erlang-gobgp
## About
**About GoBGP**

GoBGP is fast growing BGP implementation being used in various SDN/Datacenter environments. This is a open source project available from [Github GoBGP](https://github.com/osrg/gobgp). It is written in golang and kind of inherits all awesome benefits of that language and its rich features. The software developers involved in this project are quite awesome and very responsive. Thanks GoBGP folks. GoBGP uses gRPC (among other ways) to communicate with external software components.

**About erlang-gobgp**

Erlang is a powerful platform to develop and use applications. The goal of this library is to provide an abstraction to GoBGP by exposing gRPC APIs vial Erlang modules. It should be a straight forward effort to use this library to build an Erlang based BGP router.

## Start using erlang-gobgp
**Rebar Framework**
This project uses rebar3 framework, so this project can be included as a dependence to your project's rebar.config. [bgp_api.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/src/bgp_api.erl) is primary interface to the APIs exposed by this project.

    {deps, [
        {gobgp, {git, "https://github.com/vasu-dasari/erlang-gobgp.git", {branch, "master"}}}
    ]}.

### Experiment with erlang-gobgp
**Environment**

This project is developed with
 1. Erlang OTP 20.3
 2. Golang compiler (no need for this if using GoBGP binaries)
 3. Google Protocol Buffer Compiler

For convenience, a docker image with this environment is available at [vdasari/erlango:latest](https://hub.docker.com/r/vdasari/erlango/) which can be used to get a quick start.

**Testing environment**

Following components are used for creating test framework:
1. Ryu BGP router for performing interop-testing with this project.
2. mininet to create VMs which act a datacenter VMs(server farm).
3. OpenvSwitch to tie mininet to external world.

Another Docker image is provided to tie all these together at [vdasari/ovs-mn-ryu](https://hub.docker.com/r/vdasari/ovs-mn-ryu/)

A module [bgp_demo.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/apps/examples/src/bgp_demo.erl) is included as part of this project. One can take look at this module to see how project can be used.

**Test Network**

A docker based test network is used in this project to simulate server farm, edge routers, route reflectors and core routers. [Docker compose](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docker/docker-compose.yml) file details the container connectivity. The following diagram shows all elements in the network.

![Network Diagram](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docs/Network%20Diagram.jpeg)

Here [ryu1](https://github.com/vasu-dasari/erlang-gobgp/blob/master/scripts/ryu1.yml) and [ryu2](https://github.com/vasu-dasari/erlang-gobgp/blob/master/scripts/ryu2.yml) identify two PE routers and they are service server farms. ryu1 and ryu2 will be two VTEPs. erlang_gobgp, gobgp_1 are acting as route reflectors for ryu1 and ryu2 respectively. And gobgp2 will be a another core router which is used to feed routes statically for VNI 1000.

To bring up container network:

    make up # To bring up all containers
    make up gobgp_1 # To bring up only gobgp_1
Other commands that could be useful:

    make down [<container>]  # To stop and delete all containers
    make start [<container>] # To start containers
    make stop [<container>]  # To stop containers
    make connect <container> # To get to bash shell of container
    make logs <container>    # To view console logs for container

### Demo
One can start demo by bringing up test network.

    git clone https://github.com/vasu-dasari/erlang-gobgp.git
    cd erlang-gobgp
    make up # Brings up container network
    make run # Compiles the code and starts application and then lands at erlang shell

Now, to start demo:

    (gobgp@erlang_gobgp)1> bgp_demo:demo().
With erlang-gobgp framework, on can have a single instance of software running acting as a SDN BGP Controller and have it control other gobgp routers using graph APIs. In this test network, erlang_gobgp control	s all three gobgp routers. For example to control gobgp1 router, a following call can be made.

    bgp_api:server({router, gobgp1},  add, "10.0.100.2", 50051).

    
