%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2017 3:38 PM
%%%-------------------------------------------------------------------

-module(bgp_engine).
-author("vdasari").

-behaviour(gen_server).

-include("logger.hrl").
-include("gobgp_pb.hrl").
-include("bgp_api.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/3, stop/1]).
-define(SERVER, ?MODULE).
-define(EtsConfig, State#state.ets_name).

-record(state, {
    name,
    ets_name,
    ip_address = "localhost",
    port_number = 50051,
    connection = not_connected,

    connection_timer_ref
}).

-record(router_id_t, {
    key = router_id,
    router_id = not_set,
    as_number = not_set
}).

-record(neighbor_key_t, {
    ip_address      :: binary() | iolist() | undefined, %% <<"1.2.3.4">>
    as_number       :: non_neg_integer() | undefined
}).

-record(neighbor_info_t, {
    key
}).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Ip, Port) ->
    gen_server:start(?MODULE, {Name, Ip, Port}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Name, Ip, Port}) ->
    EtsName = make_ets_name(Name),
    ets:new(EtsName,[named_table, {keypos, 2}, ordered_set, public]),
    self() ! {init},
    {ok, #state{
        name = Name,
        ip_address = Ip,
        port_number = Port,
        ets_name = EtsName
    }}.

handle_call(Request, From, State) ->
    try process_call(Request, State) of
        {reply, ok, _} = Return ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, ok, Return]),
            Return;
        {reply, NotOk, _} = Return when is_atom(NotOk) ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, NotOk, Return]),
            Return;
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    From ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, From, Error, Reason, bgp_utils:pretty_print(StackTrace)]),
            {reply, Error, State}
    end.

handle_cast(Request, State) ->
    ?DEBUG("cast: Request ~p", [Request]),
    try process_cast(Request, State) of
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, Error, Reason, bgp_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    ?DEBUG("info: Request ~p", [Info]),
    try process_info_msg(Info, State) of
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Info, Error, Reason, bgp_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ?INFO("~s going down: ~p", [?MODULE, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_call({router_id,Op,Ip,AsNumber}, State) ->
    {Return,NewState} = do_router_id(Op, Ip, AsNumber, State),
    {reply, Return, NewState};
process_call({neighbor,Op,Ip,AsNumber}, State) ->
    {Return, NewState} = do_neighbor(Op, Ip, AsNumber, State),
    {reply, Return, NewState};

process_call(Request, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast({route, Op, RouteEntry}, State) ->
    Ret = do_route(Op, RouteEntry, State),
    ?INFO("Ret ~p", [Ret]),
    {noreply, State};
process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({'EXIT',_,normal}, State) ->
    {noreply, State};
process_info_msg({init},
        #state{
            ip_address = Ip,
            port_number = Port
        } = State) ->
    process_flag(trap_exit, true),
    {_,NewState} = do_server(set, Ip, Port, State),
    {noreply, NewState};

process_info_msg(retry_connection, #state{ip_address = Ip, port_number = PortNumer} = State) ->
    {ok, NewState} = do_server(set, Ip, PortNumer, State),
    {noreply, NewState};

process_info_msg({'EXIT',_,closed_by_peer}, #state{connection_timer_ref = TimerRef} = State) ->
    {noreply, State#state{connection = not_connected, connection_timer_ref = bgp_utils:restart_timer(TimerRef)}};

process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.

%%%===================================================================
%%% Worker functions
%%%===================================================================

do_server(get, _, _, #state{ip_address = OldIp, port_number = OldPort, connection = Connection} = State) ->
    {{OldIp, OldPort, Connection}, State};
do_server(set, Ip, PortNumer, #state{ip_address = OldIp, port_number = OldPort, connection = Connection} = State) when Ip == OldIp,PortNumer == OldPort,Connection /= not_connected->
    {ok, State};

do_server(Op, Ip, PortNumer, #state{connection = Connection} = State) when Connection /= not_connected ->
    grpc_client:stop_connection(Connection),
    do_server(Op, Ip,PortNumer,State#state{connection = not_connected});

do_server(set, Ip, PortNumer, #state{connection_timer_ref = TimerRef} = State) ->
    case grpc_client:connect(tcp, Ip, PortNumer) of
        {ok,Connection} ->
            gobgp_client:'StopServer'(Connection, #'StopServerRequest'{}, [{msgs_as_records,gobgp_pb}]),
            {ok, State#state{
                ip_address = Ip, port_number = PortNumer, connection = Connection,
                connection_timer_ref = bgp_utils:cancel_timer(TimerRef)}
            };
        _ ->
            {ok, State#state{
                ip_address = Ip,port_number = PortNumer, connection = not_connected, connection_timer_ref = bgp_utils:restart_timer(TimerRef)
            }}
    end.

do_router_id(get, _, _, State) ->
    [#router_id_t{router_id = RouterId, as_number = AsNumber}] = ets:lookup(?EtsConfig, router_id),
    {{RouterId, AsNumber},State};
do_router_id(start, NewRouterId, NewAsNumber, #state{connection = not_connected} = State) ->
    ets:insert(?EtsConfig, #router_id_t{router_id = NewRouterId, as_number = NewAsNumber}),
    {not_connected,State};
do_router_id(stop, _, _, #state{connection = not_connected} = State) ->
    ets:delete(?EtsConfig, router_id),
    {not_connected,State};
do_router_id(start, NewRouterId, NewAsNumber, #state{connection = Connection} = State) ->
    gobgp_client:'StopServer'(Connection, #'StopServerRequest'{}, [{msgs_as_records,gobgp_pb}]),
    Request = #'StartServerRequest'{
        global = #'Global'{
            router_id = NewRouterId,
            as = NewAsNumber
        }
    },
    {ok,_} = gobgp_client:'StartServer'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
    ets:insert(?EtsConfig, #router_id_t{router_id = NewRouterId, as_number = NewAsNumber}),
    {ok, State};
do_router_id(stop, _,_, #state{connection = Connection} = State) ->
    gobgp_client:'StopServer'(Connection, #'StopServerRequest'{}, [{msgs_as_records,gobgp_pb}]),
    ets:delete(?EtsConfig, router_id),
    {ok,State}.

do_neighbor(get, 0, 0, #state{connection = Connection} = State) ->
    {ok, #{result := Return}} =
        gobgp_client:'GetNeighbor'(Connection, #'GetNeighborRequest'{}, [{msgs_as_records, gobgp_pb}]),
    {Return, State};
do_neighbor(add, Ip, AsNumber, #state{connection = not_connected} = State) ->
    ets:insert(?EtsConfig, #neighbor_info_t{key = #neighbor_key_t{ip_address = Ip, as_number = AsNumber}}),
    {not_connected, State};
do_neighbor(delete, Ip, AsNumber, #state{connection = not_connected} = State) ->
    ets:delete(?EtsConfig, #neighbor_key_t{ip_address = Ip, as_number = AsNumber}),
    {not_connected, State};
do_neighbor(add, Ip, AsNumber, #state{connection = Connection} = State) ->
    [#router_id_t{router_id = RouterId}] = ets:lookup(?EtsConfig, router_id),
    {ok,RouteFamily} = gobgp_nif:route_family("l2vpn-evpn"),
    Request = #'AddNeighborRequest'{
        peer = #'Peer'{
            families = [RouteFamily],
            conf = #'PeerConf'{
                neighbor_address = Ip,
                peer_as = AsNumber
            },
            route_reflector = #'RouteReflector'{
                route_reflector_client = 1,
                route_reflector_cluster_id = RouterId
            }
        }
    },
    {ok,#{result := Result}} = gobgp_client:'AddNeighbor'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
    ets:insert(?EtsConfig, #neighbor_info_t{key = #neighbor_key_t{ip_address = Ip, as_number = AsNumber}}),
    {Result, State};
do_neighbor(delete, Ip, AsNumber, #state{connection = Connection} = State) ->
    Request = #'DeleteNeighborRequest'{
        peer = #'Peer'{
            conf = #'PeerConf'{
                neighbor_address = Ip,
                peer_as = AsNumber
            },
            info = #'PeerState'{
                admin_state = 'DOWN'
            }
        }
    },
    {ok, #{result := Result}} = gobgp_client:'DeleteNeighbor'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
    ets:delete(?EtsConfig, #neighbor_key_t{ip_address = Ip, as_number = AsNumber}),
    {Result, State}.

make_ets_name(Name) when is_atom(Name) ->
    erlang:list_to_atom("bgp_" ++ erlang:atom_to_list(Name));
make_ets_name(Name) when is_list(Name) ->
    erlang:list_to_atom("bgp_" ++ Name).

do_route(Op, RouteEntry, #state{connection = Connection}) ->
    {Family, NifStr} = route2nif(RouteEntry),
    ?INFO("nif: Family ~p, Command ~p", [Family, NifStr]),
    {ok,EncodedBytes} = gobgp_nif:route(Op, Family, NifStr),
    case Op of
        add ->
            Request = gobgp_pb:decode_msg(EncodedBytes, 'AddPathRequest'),
            {ok, #{result := Result}} = gobgp_client:'AddPath'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
            Result;
        delete ->
            Request = gobgp_pb:decode_msg(EncodedBytes, 'DeletePathRequest'),
            {ok, #{result := Result}} = gobgp_client:'DeletePath'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
            Result
    end.

route2nif(
        #route_entry_t{
            type = macadv
        }) ->
    {"l2vpn-evpn", "macadv aa:bb:cc:dd:ee:04 2.2.2.4 1 1 rd 64512:10 rt 64512:10 encap vxlan"};
route2nif(_RouteEntry) ->
    {"l2vpn-evpn", "macadv aa:bb:cc:dd:ee:04 2.2.2.4 1 1 rd 64512:10 rt 64512:10 encap vxlan"}.

    