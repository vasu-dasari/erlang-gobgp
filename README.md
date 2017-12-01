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

For convenience, I have a docker image with this environment at [vdasari/erlang:latest](https://hub.docker.com/r/vdasari/erlango/) which can be used to get a quick start.

**Testing environment**

I have used following components for creating test framework:
1. Ryu BGP router for performing interop-testing with this project.
2. mininet to create VMs which act a datacenter VMs.
3. Open vSwitch to tie mininet to external world.
I have another Docker image to tie all these together at [vdasari/ovs-mn-ryu](https://hub.docker.com/r/vdasari/ovs-mn-ryu/)

A module [bgp_demo.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/apps/examples/src/bgp_demo.erl) is included as part of this project. One can take look at this module to see how project can be used.

**Test Network**

A docker based test network is included in this project. [Docker compose](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docker/docker-compose.yml) file details the container connectivity. The following diagram shows all elements in the network.
![Network Diagram](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docs/Network%20Diagram.jpeg)

