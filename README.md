# erlang-gobgp
## About
**About GoBGP**

GoBGP is fast growing BGP implementation being used in various SDN/Datacenter environments. This is a open source project available from [Github GoBGP](https://github.com/osrg/gobgp). It is written in golang and kind of inherits all awesome benefits of that language and its rich features. The software developers involved in this project are quite awesome and very responsive. Thanks GoBGP folks. GoBGP uses gRPC (among other ways) to communicate with external software components.

**About erlang-gobgp**

The goal of this library is to provide an abstraction to GoBGP by exposing gRPC APIs vial Erlang modules. It should be a straight forward effort to use this library to build an Erlang based BGP router.

## Start using erlang-gobgp
**Rebar Framework**

This project uses rebar3 framework, so this project can be included as a dependency to your project's rebar.config.

    {deps, [
        {gobgp, {git, "https://github.com/vasu-dasari/erlang-gobgp.git", {branch, "master"}}}
    ]}.

Also, to be able add and delete routes, the software needs to have access to GoBGP's C-Shared library, libgobgp.so. The path to these files needs to be provided via _sys.config_ as follows:

    {gobgp, [
        {libgobgp,"/go/src/github.com/osrg/gobgp/gobgp/lib/"}
    ]}

**Development Environment**

This project is developed with
 1. Erlang OTP 20.3
 2. Golang compiler (no need for this if using GoBGP binaries)
 3. C++ Google Protocol Buffer Compiler

For convenience, a docker image with this environment is available at [vdasari/erlango:latest](https://hub.docker.com/r/vdasari/erlango/) which can be used to get a quick start.


**Software Model**

Following software component diagram shows various components involved in this design.
![Software Model](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docs/Erlang%20GoBGP%20Software%20Model%20-%20Page.jpeg)

 **API**
 
 [bgp_api.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/src/bgp_api.erl) is primary interface to the APIs exposed by this project. APIs offered by this module are not meant to replace GoBGP API, rather to
 
 1. Abstract gRPC communication infrastructure
 2. Provide notifications like route advertisements or neighbor advertisements, etc
 3. Infrastructure to be able to act as GoBGP controller, where in one can manage interact with multiple GoBGP routers from a single instance of erlang-gobgp.
 4. Provide an API from `bgp_api` if it needs to go through Nif code. For example, route add/delete code needs to go through NIF, and hence an API is provided.

The documentation provided here is to supplement browsing the code.

| API | Description |
| ------: | :------ |
|  server  |  Initiate/Stop gRPC connection to router  |
|  router_id  |  Set/Delete router-id  |
|  route  |  Add/Delete Routes  |
|  api  |  Direct API access using GoBGP Google Protocol buffer records  |

For example to use `api` method:
```erlang
-spec api(MethodName::atom, Request::tuple()) -> grpc_client_response().
api(MethodName, Request) ->
    api({router,localhost}, MethodName, Request).
```
And to use it:

```erlang
bgp_api:api('AddNeighbor',
    #'AddNeighborRequest'{
        peer = #'Peer'{
            families = [gobgp_pb:enum_value_by_symbol_Family('EVPN')],
            conf = #'PeerConf'{
                neighbor_address = list_to_binary(?Ryu1),
                peer_as = ?Ryu1_AS
            },
            route_reflector = #'RouteReflector'{
                route_reflector_client = 1,
                route_reflector_cluster_id = list_to_binary(?RouterId_Control)
            }
        }
    })
```
In above example:
'AddNeighbor' is an API as defined by [GoBGP API](https://github.com/vasu-dasari/erlang-gobgp/blob/master/proto/gobgp.proto) and 'AddNeighborRequest' is a record generated in [gobgp_pb.hrl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/include/gobgp_pb.hrl).

**Notifications**

To get BGP notifications like route advertisements or neighbor state changes, register with pg2 to get notifications of the same.

```erlang
pg2:join(gobgp_advt, self())
```
As and when a neighbor state changes is learnt following message will be sent to the registered recievers

```erlang
Source::Router which originates the message
Msg::#neighbor_advt_t{} | #route_advt_t{}
{'$gen_cast',{gobgp_advt, Source, Msg}}
```

**BGP Controller**
With erlang-gobgp framework, one can have a single instance of this software running and acting as a SDN BGP Controller and have it control other gobgp routers using GoBGP APIs. In this test network, erlang_gobgp controls all three gobgp routers. For example to control gobgp1(some name) router, a following call can be made.

```erlang
RouterInstance = {router, gobgp1},
bgp_api:server(RouterInstance, start, ?GoBGP_1, 50051),
bgp_api:router_id(RouterInstance, start, list_to_binary(?RouterId_GoBGP_1), ?GoBGP_1_AS),
bgp_api:neighbor(RouterInstance, add, list_to_binary(?Control), ?Control_AS, 'EVPN'),
```
**Demo**

There is an example [demo](https://github.com/vasu-dasari/erlang-gobgp/tree/master/apps/examples) to show a 3 node GoBGP routers interacting with a 2 Ryu based BGP nodes in the examples directory.