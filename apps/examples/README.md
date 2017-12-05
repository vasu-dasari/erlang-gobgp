Experiment with erlang-gobgp
============================

## Environment
This is a proof of concept or demo on using GoBGP software in Erlang environment. The setup is used to demonstrate BGP EVPN functionality as well.

**Components**

Following components are used for creating test framework:
1. Ryu BGP router for performing interop-testing with this project.
2. mininet to create VMs which act a datacenter VMs(server farm).
3. OpenvSwitch to tie mininet to external world.

A Docker image is provided to tie all these together at [vdasari/ovs-mn-ryu](https://hub.docker.com/r/vdasari/ovs-mn-ryu/)

A module [bgp_demo.erl](https://github.com/vasu-dasari/erlang-gobgp/blob/master/apps/examples/src/bgp_demo.erl) is included as part of this project. One can take look at this module to see how project can be used.

**Topology Setup**

A docker based test network is used in this project to simulate server farm, edge routers, route reflectors and core routers. [Docker compose](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docker/docker-compose.yml) file details the container connectivity. The following diagram shows all elements in the network.

![Network Diagram](https://github.com/vasu-dasari/erlang-gobgp/blob/master/docs/Network%20Diagram.jpeg)

Here [ryu1](https://github.com/vasu-dasari/erlang-gobgp/blob/master/scripts/ryu1.yml) and [ryu2](https://github.com/vasu-dasari/erlang-gobgp/blob/master/scripts/ryu2.yml) identify two PE routers and they are servicing server farms. ryu1 and ryu2 will be two VTEPs. erlang_gobgp, gobgp_1 are acting as route reflectors for ryu1 and ryu2 respectively. And gobgp2 will be a another core router which is used to feed routes statically for VNI 1000.

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

This function does the following:

 1. Registers to get notifications from all routers managed in this instance
 2. Starts "Control" router which is this router:
	 3. Acts as router reflector for ryu1.
	 4. Configures gobgp1 and gobgp2 as neighbors
 3. Starts "gobgp1" router:
	 4. Acts as router reflector for ryu2
	 5. Configures control and gobgp2 as neighbors
4. Starts "gobgp2" router
	5. Configure control and gobgp1 as neighbors

Once this is done, we should be seeing the neighbor and route advertisements captured in log file:
```erlang
[info] Advt: gobgp1: [{record,neighbor_advt_t},
 {neighbor_ip,<<"10.0.100.1">>},
 {admin_state,up},
 {oper_state,estbl}]
[info] Advt: gobgp1: [{record,route_advt_t},
 {type,multicast},
 {rd,<<"65000:1000">>},
 {etag,1000},
 {ip,<<"10.0.202.2">>},
 {paths,[[{record,path_advt_t},
          {oper,add},
          {asn,65000},
          {neighbor_ip,<<"10.0.202.2">>}]]}]
```

**Start Ryu Setups**

Connect to ryu setups and configure those nodes.
```
$ make connect ryu1
root@ryu1: # ./ryu1.sh
Starting topology from ryu1.yml
.
.
sudo ryu-manager ryu.app.rest_vtep ryu.app.ofctl_rest
Router id 10.0.201.2, as_number 65000
Neighbor Id 10.0.201.1 as_number 65000
Adding endpoint: Port s1-eth1 VNI 1000, IP 10.0.1.1, MAC 02:00:0a:00:01:01
Adding endpoint: Port s1-eth2 VNI 2000, IP 10.0.2.1, MAC 02:00:0a:00:02:01
Adding endpoint: Port s1-eth3 VNI 3000, IP 10.0.3.1, MAC 02:00:0a:00:03:01
Adding endpoint: Port s1-eth4 VNI 4000, IP 10.0.4.1, MAC 02:00:0a:00:04:01
Adding endpoint: Port s1-eth5 VNI 5000, IP 10.0.5.1, MAC 02:00:0a:00:05:01
*** Starting CLI:
mininet>
```

**GoBGP State**
Neighbor state
```
root@erlang_gobgp:erlang-gobgp/scripts# gobgp neighbor
Peer          AS  Up/Down State       |#Received  Accepted
10.0.100.2 65000 00:05:02 Establ      |       11        11
10.0.100.3 65000 00:05:00 Establ      |        0         0
10.0.201.2 65000 00:04:57 Establ      |       10        10
```
And routing table state:
```
root@erlang_gobgp:erlang-gobgp/scripts# gobgp global rib -a evpn
   Network                                                                  Labels     Next Hop             AS_PATH              Age        Attrs
*> [type:macadv][rd:65000:1000][etag:0][mac:02:00:0a:00:01:01][ip:10.0.1.1] [1000]     10.0.201.2                                00:05:25   [{Origin: ?} {LocalPref: 100} {Extcomms: [65000:1000], [VXLAN]} [ESI: single-homed]]
*> [type:macadv][rd:65000:1000][etag:0][mac:02:00:0a:00:01:02][ip:10.0.1.2] [1000]     10.0.202.2                                00:05:26   [{Origin: ?} {LocalPref: 100} {Extcomms: [65000:1000], [VXLAN]} [ESI: single-homed]]
.
.
*> [type:multicast][rd:65000:5000][etag:5000][ip:10.0.202.2]                           10.0.202.2                                00:05:26   [{Origin: ?} {LocalPref: 100} {Extcomms: [65000:5000]}]
```
**Ping**
GoBGP software does not install kernel routes. It is application's responsibility to do the same. To install the routes to be able to do the pings, attached ./erlang_gobgp.sh, gobgp_1.sh and gobgp_2.sh files in scripts directory, need to execute those on respective docker containers.

From ryu1 window one can ping 10.0.1.2 which on cluster ryu2.
```
mininet> r0 ping 10.0.1.2
PING 10.0.1.2 (10.0.1.2) 56(84) bytes of data.
64 bytes from 10.0.1.2: icmp_seq=1 ttl=64 time=0.873 ms
.
```
And when we do a tcpdump on eth1 on ryu* interface eth1 or anywhere else on gobgp nodes, we can see that the packets are VxLAN tagged.