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

-include("logger.hrl").
-include("bgp_api.hrl").

%% API
-export([demo/0]).

demo() ->
    ?INFO("server(set, localhost, 50051) => ~n~p", [bgp_api:server(set, "localhost", 50051)]),
    ?INFO("router_id(start, <<10.0.123.100>>, 65001) => ~n~p", [bgp_api:router_id(start, <<"10.0.123.110">>, 65001)]),
    ?INFO("router_id(get, 0,0) => ~n~p", [bgp_api:router_id(get, 0,0)]),
    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(delete, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(delete, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~p", [bgp_api:neighbor(get,0,0)]),
    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [bgp_api:neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~s", [bgp_utils:pretty_print(bgp_api:neighbor(get,0,0))]),
    ?INFO("neighbor(get,0,0) => ~n~s", [bgp_api:route(add,#route_entry_t{})]).

