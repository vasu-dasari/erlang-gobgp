%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2017 3:38 PM
%%%-------------------------------------------------------------------
-module(bgp_api).
-author("vdasari").

-behaviour(gen_server).

-include("logger.hrl").
-include("gobgp_pb.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([server/3, router_id/3, neighbor/3, demo/0, pretty_print/1]).
-define(SERVER, ?MODULE).
-define(EtsConfig, bgp_ets_config).

-record(state, {
    ip_address = 0,
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

-define(cast(A),gen_server:cast(?MODULE, A)).
-define(call(A),gen_server:call(?MODULE, A)).

server(Op, Ip, Port) ->
    ?call({server, Op, Ip, Port}).

router_id(Op, RouterId, AsNumber) ->
    ?call({router_id, Op, RouterId, AsNumber}).

neighbor(Op, Ip, AsNumber) ->
    ?call({neighbor, Op, Ip, AsNumber}).

demo() ->
    ?INFO("server(set, localhost, 50051) => ~n~p", [server(set, "localhost", 50051)]),
    ?INFO("router_id(start, <<10.0.123.100>>, 65001) => ~n~p", [router_id(start, <<"10.0.123.110">>, 65001)]),
    ?INFO("router_id(get, 0,0) => ~n~p", [router_id(get, 0,0)]),
    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(delete, <<10.0.123.200>>, 65002) => ~n~p", [neighbor(delete, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~p", [neighbor(get,0,0)]),
    ?INFO("neighbor(add, <<10.0.123.200>>, 65002) => ~n~p", [neighbor(add, <<"10.0.123.200">>, 65002)]),
    ?INFO("neighbor(get,0,0) => ~n~s", [pretty_print(neighbor(get,0,0))]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! {init},
    {ok, #state{}}.

handle_call(Request, From, State) ->
    try process_call(Request, State) of
        {reply, ok, _} = Return ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, ok, Request]),
            Return;
        {reply, NotOk, _} = Return when is_atom(NotOk) ->
            ?INFO("call: Request From ~p, Returns ~p~n~p", [From, NotOk, Request]),
            Return;
        Return ->
            Return
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR("Failed:~n    Request ~p~n    From ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, From, Error, Reason, pretty_print(StackTrace)]),
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
                [Request, Error, Reason, pretty_print(StackTrace)]),
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
                [Info, Error, Reason, pretty_print(StackTrace)]),
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
process_call({server,Op,Ip,PortNumer}, State) ->
    {Return,NewState} = do_server(Op, Ip, PortNumer, State),
    {reply, Return, NewState};
process_call({router_id,Op,Ip,AsNumber}, State) ->
    {Return,NewState} = do_router_id(Op, Ip, AsNumber, State),
    {reply, Return, NewState};
process_call({neighbor,Op,Ip,AsNumber}, State) ->
    {Return, NewState} = do_neighbor(Op, Ip, AsNumber, State),
    {reply, Return, NewState};

process_call(Request, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({'EXIT',_,normal}, State) ->
    {noreply, State};
process_info_msg({init}, State) ->
    ets:new(?EtsConfig,[named_table, {keypos, 2}, ordered_set, public]),
    {noreply, State};

process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.

%%%===================================================================
%%% Worker functions
%%%===================================================================

do_server(get, _, _, #state{ip_address = OldIp, port_number = OldPort, connection = Connection} = State) ->
    {{OldIp, OldPort, Connection}, State};
do_server(set, Ip, PortNumer, #state{ip_address = OldIp, port_number = OldPort} = State) when Ip == OldIp,PortNumer == OldPort ->
    {ok, State};

do_server(Op, Ip, PortNumer, #state{connection = Connection} = State) when Connection /= not_connected ->
    grpc_client:stop_connection(Connection),
    do_server(Op, Ip,PortNumer,State#state{connection = not_connected});

do_server(set, Ip, PortNumer, #state{connection_timer_ref = TimerRef} = State) ->
    case grpc_client:connect(tcp, Ip, PortNumer) of
        {ok,Connection} ->
            gobgp_client:'StopServer'(Connection, #'StopServerRequest'{}, [{msgs_as_records,gobgp_pb}]),
            {ok, State#state{
                ip_address = Ip, port_number = PortNumer, connection = Connection, connection_timer_ref = cancel_timer(TimerRef)}
            };
        _ ->
            {ok, State#state{
                ip_address = Ip,port_number = PortNumer, connection = not_connected, connection_timer_ref = restart_timer(TimerRef)
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
    Request = #'AddNeighborRequest'{
        peer = #'Peer'{
            conf = #'PeerConf'{
                neighbor_address = Ip,
                peer_as = AsNumber
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

cancel_timer(undefined) -> undefined;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref), undefined.
restart_timer(TimerRef) ->
    cancel_timer(TimerRef),
    erlang:send_after(5000, self(), retry_connection).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).
