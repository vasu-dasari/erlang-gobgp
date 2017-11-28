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
-export([demo/0, demo1/0, setup/2, ryu/0, refresh/0, route/1, handle_advts/0, vrf/1]).

-define(Control, "10.0.123.10").
-define(GoBGP_1, "10.0.123.100").
-define(GoBGP_2, "10.0.123.200").

-define(RouterId_Control, "1.1.1.10").
-define(RouterId_GoBGP_1, "1.1.1.100").
-define(RouterId_GoBGP_2, "1.1.1.200").

demo() ->
    %% Setup control BGP Session
    setup(?Control, ?RouterId_Control),
    setup(?GoBGP_1, ?RouterId_GoBGP_1),
    setup(?GoBGP_2, ?RouterId_GoBGP_2),
    bgp_api:route({router,?Control}, add,
        #route_entry_t{
            type = macadv,
            mac_address = "aa:bb:cc:dd:ee:04",
            ip_address = "3.3.3.1",
            service_id = 1000,
            rd = "64512:10",
            rt = "64512:10",
            encap = vxlan,
            family = 'EVPN'
        }).

demo1() ->

    ?INFO("server(set, localhost, 50051) => ~n~p", [bgp_api:server(set, "localhost", 50051)]),
    ?INFO("router_id(start, <<10.0.123.100>>, 65001) => ~n~p", [bgp_api:router_id(start, <<"10.0.123.100">>, 65001)]),
    ?INFO("neighbor(add, <<10.0.123.100>>, 65001) => ~n~p", [bgp_api:neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("router_id(get, 0,0) => ~n~p", [bgp_api:router_id(get, 0,0)]),

    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(delete, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(delete, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~p", [bgp_api:neighbor(get,0,0)]),
    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~s", [bgp_utils:pretty_print(bgp_api:neighbor(get,0,0))]),
    ?INFO("neighbor(get,0,0) => ~n~s", [bgp_api:route(add,#route_entry_t{})]).

setup(Container, RouterId) ->

    RouterInstance = {router, Container},
    
    bgp_api:server(RouterInstance, set, Container, 50051),
    bgp_api:router_id(RouterInstance, start, list_to_binary(RouterId), 65001),

    case Container == ?Control of
        true -> ok;
        _ -> bgp_api:neighbor(RouterInstance, add, <<?Control>>, 65001, 'EVPN')
    end,

    case Container == ?GoBGP_1 of
        true -> ok;
        _ -> bgp_api:neighbor(RouterInstance, add, <<?GoBGP_1>>, 65001, 'EVPN')
    end,

    case Container == ?GoBGP_2 of
        true -> ok;
        _ -> bgp_api:neighbor(RouterInstance, add, <<?GoBGP_2>>, 65001, 'EVPN')
    end.

ryu() ->

    Container = "10.0.124.30",
    RouterId = "10.0.124.30",
    Neighbor = "10.0.124.20",

    RouterInstance = {router, Container},

    handle_advts(),

    bgp_api:server(RouterInstance, set, Container, 50051),
    bgp_api:router_id(RouterInstance, start, list_to_binary(RouterId), 65000),

    bgp_api:neighbor(RouterInstance, add, list_to_binary(Neighbor), 65000, 'EVPN'),

    bgp_api:api(RouterInstance, 'AddVrf',
        #'AddVrfRequest'{
            vrf = #'Vrf'{
                name = <<"vrf1000">>,
                id = 1000,
                rd = bgp_utils:rdrt2binary("65000:1000"),
                import_rt = [bgp_utils:rdrt2binary("65000:1000")],
                export_rt = [bgp_utils:rdrt2binary("65000:1000")]
            }
        }),
    bgp_api:route(RouterInstance, add,
        #route_entry_t{
            type = macadv,
            mac_address = "aa:bb:cc:dd:ee:04",
            ip_address = "10.0.1.3",
            service_id = 1000,
            rd = "65000:1000",
            rt = "65000:1000",
            encap = vxlan,
            family = 'EVPN'
        }).


refresh() ->
    bgp_api:neighbor({router, "10.0.124.30"}, delete, list_to_binary("10.0.124.20"), 65000, 'EVPN'),
    bgp_api:neighbor({router, "10.0.124.30"}, add, list_to_binary("10.0.124.20"), 65000, 'EVPN').

route(Op) ->
    Container = "10.0.124.30",
    bgp_api:route({router,Container}, Op,
        #route_entry_t{
            type = macadv,
            mac_address = "aa:bb:cc:dd:ee:04",
            ip_address = "10.0.1.3",
            service_id = 1000,
            rd = "65000:1000",
            rt = "65000:1000",
            encap = vxlan,
            family = 'EVPN'
        }).

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
        {'$gen_cast',{bgp_engine, gobgp_advt, Msg}} ->
            ?INFO("Advt: ~s", [bgp_utils:record_to_proplist(to_str, Msg)]),
            rx_advt_loop();
        Msg ->
            ?INFO("Unknown Advt received, so quitting: ~p", [Msg])
    end.
