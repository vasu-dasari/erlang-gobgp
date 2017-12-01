%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2017 9:53 PM
%%%-------------------------------------------------------------------
-module(bgp_demo).
-author("vdasari").

-include_lib("logger.hrl").
-include_lib("bgp_api.hrl").
-include("gobgp_pb.hrl").

%% API
-export([demo/0, control/0, refresh/0, route/1, handle_advts/0, vrf/1, gobgp1/0, gobgp2/0, zebra/1]).

-define(Control, "10.0.100.1").
-define(GoBGP_1, "10.0.100.2").
-define(GoBGP_2, "10.0.100.3").
-define(Ryu1, "10.0.201.2").
-define(Ryu2, "10.0.202.2").

-define(Control_AS, 65000).
-define(GoBGP_1_AS, 65000).
-define(GoBGP_2_AS, 65000).
-define(Ryu1_AS, 65000).
-define(Ryu2_AS, 65000).

-define(RouterId_Control, "1.1.1.10").
-define(RouterId_GoBGP_1, "1.1.1.100").
-define(RouterId_GoBGP_2, "1.1.1.200").

demo() ->
    handle_advts(),

    control(),
    gobgp1(),
    gobgp2(),
    ok.

control() ->
    RouterInstance = {router, control},

    bgp_api:server(RouterInstance, set, ?Control, 50051),
    bgp_api:router_id(RouterInstance, start, list_to_binary(?RouterId_Control), ?Control_AS),

    bgp_api:api(RouterInstance, 'AddNeighbor',
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
        }),
    bgp_api:neighbor(RouterInstance, add, list_to_binary(?GoBGP_1), ?GoBGP_1_AS, 'EVPN'),
    bgp_api:neighbor(RouterInstance, add, list_to_binary(?GoBGP_2), ?GoBGP_2_AS, 'EVPN'),
    ok.

gobgp1() ->
    RouterInstance = {router, gobgp1},

    bgp_api:server(RouterInstance, set, ?GoBGP_1, 50051),
    bgp_api:router_id(RouterInstance, start, list_to_binary(?RouterId_GoBGP_1), ?GoBGP_1_AS),

    bgp_api:api(RouterInstance, 'AddNeighbor',
        #'AddNeighborRequest'{
            peer = #'Peer'{
                families = [gobgp_pb:enum_value_by_symbol_Family('EVPN')],
                conf = #'PeerConf'{
                    neighbor_address = list_to_binary(?Ryu2),
                    peer_as = ?Ryu2_AS
                },
                route_reflector = #'RouteReflector'{
                    route_reflector_client = 1,
                    route_reflector_cluster_id = list_to_binary(?RouterId_GoBGP_1)
                }
            }
        }),
    bgp_api:neighbor(RouterInstance, add, list_to_binary(?Control), ?Control_AS, 'EVPN'),
    bgp_api:neighbor(RouterInstance, add, list_to_binary(?GoBGP_2), ?GoBGP_2_AS, 'EVPN'),
    ok.

gobgp2() ->
    RouterInstance = {router, gobgp2},

    bgp_api:server(RouterInstance, set, ?GoBGP_2, 50051),
    bgp_api:router_id(RouterInstance, start, list_to_binary(?RouterId_GoBGP_2), ?GoBGP_2_AS),

    bgp_api:neighbor(RouterInstance, add, list_to_binary(?Control), ?Control_AS, 'EVPN'),
    bgp_api:neighbor(RouterInstance, add, list_to_binary(?GoBGP_1), ?GoBGP_1_AS, 'EVPN'),

    ok.

refresh() ->
    bgp_api:neighbor({router, "10.0.124.30"}, delete, list_to_binary("10.0.124.20"), 65000, 'EVPN'),
    bgp_api:neighbor({router, "10.0.124.30"}, add, list_to_binary("10.0.124.20"), 65000, 'EVPN').

route(Op) ->
    RouterInstance = {router, gobgp2},
    bgp_api:route(RouterInstance, Op,
        #route_entry_t{
            type = macadv,
            mac_address = "aa:bb:cc:dd:ee:04",
            ip_address = "10.0.1.3",
            service_id = 1000,
            rd = "65000:1000",
            rt = "65000:1000",
            encap = vxlan,
            family = 'EVPN',
            nexthop = "10.0.203.2"
        }),
    bgp_api:route(RouterInstance, Op,
        #route_entry_t{
            type = multicast,
            service_id = 1000,
            rd = "65000:1000",
            rt = "65000:1000",
            encap = vxlan,
            family = 'EVPN',
            nexthop = "10.0.203.2"
        }),

    ok.

vrf(Op) ->
    Vrf = #'Vrf'{
        name = <<"vrf1000">>,
        id = 1000,
        rd = bgp_utils:rdrt2binary("65000:1000"),
        import_rt = [bgp_utils:rdrt2binary("65000:1000")],
        export_rt = [bgp_utils:rdrt2binary("65000:1000")]
    },
    case Op of
        add ->
            bgp_api:api({router,"10.0.124.30"}, 'AddVrf',
                #'AddVrfRequest'{
                    vrf = Vrf
                });
        delete ->
            bgp_api:api({router,"10.0.124.30"}, 'DeleteVrf',
                #'DeleteVrfRequest'{
                    vrf = Vrf
                })
    end.

handle_advts() ->
    spawn(fun() ->
        pg2:create(gobgp_advt),
        pg2:join(gobgp_advt, self()),
        rx_advt_loop()
    end).

rx_advt_loop() ->
    receive
        {'$gen_cast',{gobgp_advt, Source, Msg}} ->
            ?INFO("Advt: ~p: ~s", [Source, bgp_utils:record_to_proplist(to_str, Msg)]),
            rx_advt_loop();
        Msg ->
            ?INFO("Unknown Advt received, so quitting: ~p", [Msg])
    end.

zebra(Container) ->
    RouterInstance = {router,Container},
    bgp_api:api(RouterInstance, 'EnableZebra',
        #'EnableZebraRequest'{
            url = <<"unix:/var/run/quagga/zserv.api">>,
            route_types = [<<"static">>, <<"connect">>]
        }),
    ok.