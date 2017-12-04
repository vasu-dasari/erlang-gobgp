# erlang-gobgp
## About
**About GoBGP**

GoBGP is fast growing BGP implementation being used in various SDN/Datacenter environments. This is a open source project available from [Github GoBGP](https://github.com/osrg/gobgp). It is written in golang and kind of inherits all awesome benefits of that language and its rich features. The software developers involved in this project are quite awesome and very responsive. Thanks GoBGP folks. GoBGP uses gRPC (among other ways) to communicate with external software components.

**About erlang-gobgp**

Erlang is a powerful platform to develop and use applications. The goal of this library is to provide an abstraction to GoBGP by exposing gRPC APIs vial Erlang modules. It should be a straight forward effort to use this library to build an Erlang based BGP router.

## Start using erlang-gobgp
**Rebar Framework**

This project uses rebar3 framework, so this project can be included as a dependency to your project's rebar.config. [bgp_api.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/src/bgp_api.erl) is primary interface to the APIs exposed by this project.

    {deps, [
        {gobgp, {git, "https://github.com/vasu-dasari/erlang-gobgp.git", {branch, "master"}}}
    ]}.

Also, to be able add and delete routes, the software needs to have access to GoBGP's C-Shared library, libgobgp.so. The path to these files needs to be provided via _sys.config_ as follows:

    {gobgp, [
        {libgobgp,"/go/src/github.com/osrg/gobgp/gobgp/lib/"}
    ]}
 
 **API**
 
 bgp_api: