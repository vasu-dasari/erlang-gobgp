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
    as_number = not_set,
    peer_state_stream,
    rib_stream}).

-record(neighbor_key_t, {
    ip_address      :: binary() | iolist() | undefined, %% <<"1.2.3.4">>
    as_number       :: non_neg_integer() | undefined
}).

-record(neighbor_info_t, {
    key,
    family          :: gobgp_pb:'Family'(),
    route_reflector :: binary() | iolist() | undefined
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
process_call({neighbor,Op,Ip,AsNumber, Family}, State) ->
    {Return, NewState} = do_neighbor(Op, Ip, AsNumber, Family, State),
    {reply, Return, NewState};
process_call({api, Method, Request}, State) ->
    {reply, do_api(Method, Request, State), State};
process_call(status, State) ->
    do_status(State),
    {reply, ok, State};

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
    pg2:create(gobgp_advt),
    process_flag(trap_exit, true),
    {_,NewState} = do_server(set, Ip, Port, State),
    {noreply, NewState};

process_info_msg(retry_connection, #state{ip_address = Ip, port_number = PortNumer} = State) ->
    {ok, NewState} = do_server(set, Ip, PortNumer, State),
    {noreply, NewState};

process_info_msg({'EXIT',_,closed_by_peer}, #state{connection_timer_ref = TimerRef} = State) ->
    {noreply, State#state{connection = not_connected, connection_timer_ref = bgp_utils:restart_timer(TimerRef)}};
process_info_msg({_,{data,Message}}, State) ->
    do_notify(State#state.name, Message),
    {noreply, State};

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

    {PeerStream, RibStream} = do_monitor(Connection),

    ets:insert(?EtsConfig, #router_id_t{
        router_id = NewRouterId, as_number = NewAsNumber,
        peer_state_stream = PeerStream,
        rib_stream = RibStream
    }),
    {ok, State};
do_router_id(stop, _,_, #state{connection = Connection} = State) ->
    gobgp_client:'StopServer'(Connection, #'StopServerRequest'{}, [{msgs_as_records,gobgp_pb}]),
    ets:delete(?EtsConfig, router_id),
    {ok,State}.

do_monitor(Connection) ->
    {ok, PeerStream} = grpc_client:new_stream(Connection, 'GobgpApi', 'MonitorPeerState',
        gobgp, [
            {msgs_as_records, gobgp_pb},
            {async_notification, self()}
        ]),
    grpc_client:send(PeerStream, #'Arguments'{name = ""}),

    {ok, RibStream} = grpc_client:new_stream(Connection, 'GobgpApi', 'MonitorRib',
        gobgp, [
            {msgs_as_records, gobgp_pb},
            {async_notification, self()}
        ]),
    grpc_client:send(RibStream,
        #'MonitorRibRequest'{
            table = #'Table'{
                type = 'GLOBAL',
                family = gobgp_pb:enum_value_by_symbol_Family('EVPN')
            }
        }),
    {PeerStream, RibStream}.

do_neighbor(get, 0, 0, _, #state{connection = Connection} = State) ->
    {ok, #{result := Return}} =
        gobgp_client:'GetNeighbor'(Connection, #'GetNeighborRequest'{}, [{msgs_as_records, gobgp_pb}]),
    {Return, State};
do_neighbor(add, Ip, AsNumber, Family, #state{connection = not_connected} = State) ->
    ets:insert(?EtsConfig, #neighbor_info_t{
        key = #neighbor_key_t{ip_address = Ip, as_number = AsNumber}, family = Family
    }),
    {not_connected, State};
do_neighbor(delete, Ip, AsNumber, _, #state{connection = not_connected} = State) ->
    ets:delete(?EtsConfig, #neighbor_key_t{ip_address = Ip, as_number = AsNumber}),
    {not_connected, State};
do_neighbor(add, Ip, AsNumber, Family, #state{connection = Connection} = State) ->
    Request = #'AddNeighborRequest'{
        peer = #'Peer'{
            families = [gobgp_pb:enum_value_by_symbol_Family(Family)],
            conf = #'PeerConf'{
                neighbor_address = Ip,
                peer_as = AsNumber
            }
        }
    },
    {ok,#{result := Result}} = gobgp_client:'AddNeighbor'(Connection, Request, [{msgs_as_records, gobgp_pb}]),
    ets:insert(?EtsConfig, #neighbor_info_t{key = #neighbor_key_t{ip_address = Ip, as_number = AsNumber}}),
    {Result, State};
do_neighbor(delete, Ip, AsNumber, _,  #state{connection = Connection} = State) ->
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

do_api(MethodName, Request, State) ->
    {ok, #{result := Result}} = gobgp_client:MethodName(
        State#state.connection, Request, [{msgs_as_records, gobgp_pb}]),
    Result.

make_ets_name(Name) when is_atom(Name) ->
    erlang:list_to_atom("bgp_" ++ erlang:atom_to_list(Name));
make_ets_name(Name) when is_list(Name) ->
    erlang:list_to_atom("bgp_" ++ Name).

do_route(Op, RouteEntry, #state{connection = Connection}) ->
    {Family, NifStr} = route2nif(RouteEntry),
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
            type = macadv,
            mac_address = Mac
        } = E) ->
    NifStr =
        io_lib:format("macadv ~s ~s ~p ~p rd ~s rt ~s encap ~p -a ~p", [
            Mac,
            E#route_entry_t.ip_address,
            E#route_entry_t.service_id,
            E#route_entry_t.service_id,
            E#route_entry_t.rd,
            E#route_entry_t.rt,
            E#route_entry_t.encap,
            E#route_entry_t.encap
        ]),
    ?DEBUG("NifStr ~s", [lists:flatten(NifStr)]),
    {"l2vpn-evpn", lists:flatten(NifStr)};
route2nif(_RouteEntry) ->
    {"l2vpn-evpn", "macadv aa:bb:cc:dd:ee:04 2.2.2.4 1 1 rd 65001:10 rt 65001:10 encap vxlan -a evpn"}.

do_status(State) ->
    [#router_id_t{
        peer_state_stream = PeerStream,
        rib_stream = RibStream
    }] = ets:lookup(?EtsConfig, router_id),
    ?INFO("Peer State ~p", [grpc_client:get(PeerStream)]),
    ?INFO("Rib State ~p", [grpc_client:get(RibStream)]),
    ?INFO("Peer State ~p", [grpc_client:rcv(PeerStream, 1000)]),
    ?INFO("Rib State ~p", [grpc_client:rcv(RibStream, 1000)]),
    _X = State,
    ok.

do_notify(Instance,
        #'Peer'{
            conf = #'PeerConf'{
                neighbor_address = Ip
            },
            info = #'PeerState'{
                bgp_state = BgpState,
                admin_state = AdminState
            }
        }) ->
    NeighborAdvt = #neighbor_advt_t{
        neighbor_ip = Ip,
        admin_state = case AdminState of 'UP' -> up; 'DOWN' -> down end,
        oper_state = get_bgp_state(BgpState)
    },
    do_announce(Instance, NeighborAdvt),
    ?DEBUG("Neighbor announcement ~p", [NeighborAdvt]);
do_notify(Instance,
        #'Destination'{
            prefix = Prefix,
            paths = Paths

        }) ->
    PathsAdvt = lists:foldl(fun
        (#'Path'{
            is_withdraw = IsWithDraw,
            source_asn = SourceAsn,
            neighbor_ip = NeighborIp
        }, Acc) ->
            [
                #path_advt_t{
                    oper = case IsWithDraw of true -> delete; _ -> add end,
                    asn = SourceAsn,
                    neighbor_ip = NeighborIp
                } | Acc]
    end, [], Paths),

    RouteAdvt = lists:foldl(fun
        (Tuple, Acc) ->
            extract_prefix_tuple(Tuple, Acc)
    end, #route_advt_t{paths = PathsAdvt}, string:tokens(binary_to_list(Prefix), "[]")),

    do_announce(Instance, RouteAdvt),

    ?DEBUG("Route Announcement ~p", [RouteAdvt]),
    ok;
do_notify(_, Msg) ->
    ?INFO("Unknown notification ~p", [Msg]).

do_announce(Instance, Advt) ->
    case pg2:get_local_members(gobgp_advt) of
        {error, _} ->
            ok;
        Pids ->
            [gen_server:cast(Pid, {gobgp_advt, Instance, Advt}) || Pid <- Pids]
    end.

get_bgp_state(BgpState) ->
    case BgpState of
        <<"BGP_FSM_IDLE">> -> idle;
        <<"BGP_FSM_CONNECT">> -> connect;
        <<"BGP_FSM_ACTIVE">> -> active;
        <<"BGP_FSM_OPENSENT">> -> open_sent;
        <<"BGP_FSM_OPENCONFIRM">> -> open_confirm;
        <<"BGP_FSM_ESTABLISHED">> -> estbl
    end.

extract_prefix_tuple("type:" ++ ValueStr, RouteAdvt) ->
    RouteAdvt#route_advt_t{type = list_to_atom(ValueStr)};
extract_prefix_tuple("rd:" ++ ValueStr, RouteAdvt) ->
    RouteAdvt#route_advt_t{rd = list_to_binary(ValueStr)};
extract_prefix_tuple("etag:" ++ ValueStr, RouteAdvt) ->
    RouteAdvt#route_advt_t{etag = list_to_integer(ValueStr)};
extract_prefix_tuple("mac:" ++ ValueStr, RouteAdvt) ->
    RouteAdvt#route_advt_t{mac =  list_to_binary(ValueStr)};
extract_prefix_tuple("ip:" ++ ValueStr, RouteAdvt) ->
    RouteAdvt#route_advt_t{ip = list_to_binary(ValueStr)};
extract_prefix_tuple(Tuple, RouteAdvt) ->
    ?INFO("Unhandled prefix tuple: ~p", [Tuple]),
    RouteAdvt.