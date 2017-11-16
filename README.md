# erlang-gobgp
This is a library, which can be a wrapper for gobgp and an interface for Erlang based applications to be able act as BGP routers. This library uses GoBGP APIs defined in gobgp.proto and GRPC for communication with gobgp.
Goal of this library is to provide
* A  minimal set of functions via API and absorb the complexity of configuring BGP.
* Provide a set of APIs to be able to configure EVPN in GoBGP. 

## TODO
Add NIFs to serialize calls to gobgp to be able advertise EVPN routes
## Build
To be able to build this project and test drive one needs to have docker installed on their host machine. The build process will create a docker container based on a image vdasari/erlango from Docker hub. This Docker image contains Erlang 20.3 OTP and golang 1.9.2.
To compile and execute the application, just do 'make run'. This will 
1. Download the docker image onto your local machine
2. Starts the container "gobgp" 
3. Then compiles the project
4. Launches erlang shell

Once Erlang shell appears, one can call bgp_api:demo() which is a test driving function to be able to:
* Connect to gobgp daemon running on local container.
* Start the router id.
* Add a neighbor.
* Get current neighbor list.
* Delete a neighbor.

## Erlang Screenhot
```
$ make run
(gobgp@258a817c534e)1> bgp_demo:demo().

15:58:08.091 [info] server(set, localhost, 50051) =>
ok
15:58:08.179 [info] router_id(start, <<10.0.123.100>>, 65001) =>
ok
15:58:08.179 [info] router_id(get, 0,0) =>
{<<"10.0.123.110">>,65001}
15:58:08.223 [info] neighbor(add, <<10.0.123.200>>, 65002) =>
{'AddNeighborResponse'}
15:58:08.266 [info] neighbor(delete, <<10.0.123.200>>, 65002) =>
{'DeleteNeighborResponse'}
15:58:08.310 [info] neighbor(get,0,0) =>
{'GetNeighborResponse',[]}
15:58:08.354 [info] neighbor(add, <<10.0.123.200>>, 65002) =>
{'AddNeighborResponse'}
15:58:08.400 [info] neighbor(get,0,0) =>
{'GetNeighborResponse',
    [{'Peer',
         [65537],
         {'ApplyPolicy',
             {'PolicyAssignment','IN','GLOBAL',<<>>,[],-1},
             {'PolicyAssignment','EXPORT','GLOBAL',<<>>,[],-1},
             {'PolicyAssignment','IMPORT','GLOBAL',<<>>,[],-1}},
         {'PeerConf',<<>>,<<>>,65001,<<"10.0.123.200">>,65002,<<>>,1,'NONE',
             false,0,[],
             [<<2,0>>,<<1,4,0,1,0,1>>,<<65,4,0,0,253,233>>],
             <<>>,[],<<"0.0.0.0">>,<<>>,<<>>,0,false},
         undefined,
         {'RouteReflector',false,<<>>},
         {'PeerState',<<>>,<<>>,0,
             {'Messages',{'Message',0,0,0,0,0,0,0},{'Message',0,0,0,0,0,0,0}},
             <<"10.0.123.200">>,65002,<<>>,1,undefined,0,false,0,0,[],
             <<"active">>,'UP',0,0,0,0,0},
         {'Timers',
             {'TimersConfig',120,90,30,0},
             {'TimersState',0,0,0,0,0,0,1509638288}},
         {'Transport',<<"0.0.0.0">>,0,false,false,<<>>,0,0},
         {'RouteServer',false},
         {'GracefulRestart',false,0,false,0,false,false},
         [{'AfiSafi',
              {'MpGracefulRestart',
                  {'MpGracefulRestartConfig',false},
                  undefined},
              {'AfiSafiConfig',65537,true},
              {'ApplyPolicy',
                  {'PolicyAssignment','IN','GLOBAL',<<>>,[],-1},
                  {'PolicyAssignment','EXPORT','GLOBAL',<<>>,[],-1},
                  {'PolicyAssignment','IMPORT','GLOBAL',<<>>,[],-1}},
              {'RouteSelectionOptions',
                  {'RouteSelectionOptionsConfig',false,false,false,false,
                      false,false},
                  undefined},
              {'UseMultiplePaths',
                  {'UseMultiplePathsConfig',false},
                  undefined,
                  {'Ebgp',{'EbgpConfig',false,0},undefined},
                  {'Ibgp',{'IbgpConfig',0},undefined}},
              undefined,
              {'RouteTargetMembership',
                  {'RouteTargetMembershipConfig',0},
                  undefined},
              {'LongLivedGracefulRestart',
                  {'LongLivedGracefulRestartConfig',false,0},
                  undefined},
              {'AddPaths',{'AddPathsConfig',false,0},undefined}}],
         {'AddPaths',{'AddPathsConfig',false,0},undefined}}]}
ok
```
## GoBGP Screenshot
Launch bash shell to the container by doing make shell from any window. One can see that gobgp is configured with the parameters configured via bgp_api:demo().
```
$ make shell
root@4c66c9f6474c:~# gobgp global
AS:        65001
Router-ID: 10.0.123.110
Listening Port: 179, Addresses: 0.0.0.0, ::
root@4c66c9f6474c:~# gobgp neighbor
Peer            AS Up/Down State       |#Received  Accepted
10.0.123.200 65002   never Active      |        0         0
root@4c66c9f6474c:~# gobgp neighbor 10.0.123.200
BGP neighbor is 10.0.123.200, remote AS 65002
  BGP version 4, remote router ID unknown
  BGP state = active, up for 17472d 19:55:20
  BGP OutQ = 0, Flops = 0
  Hold time is 0, keepalive interval is 0 seconds
  Configured hold time is 90, keepalive interval is 30 seconds

  Neighbor capabilities:
    multiprotocol:
        ipv4-unicast:	advertised
    route-refresh:	advertised
    4-octet-as:	advertised
  Message statistics:
                         Sent       Rcvd
    Opens:                  0          0
    Notifications:          0          0
    Updates:                0          0
    Keepalives:             0          0
    Route Refresh:          0          0
    Discarded:              0          0
    Total:                  0          0
  Route statistics:
    Advertised:             0
    Received:               0
    Accepted:               0
root@4c66c9f6474c:~#
```