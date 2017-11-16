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
-export([server/3, router_id/3, neighbor/3, route/2, route_reflector/2]).
-export([server/4, router_id/4, neighbor/4, route/3, route_reflector/3]).
-export([]).
-define(SERVER, ?MODULE).
-define(EtsConfig, bgp_sessions_ets).

-record(state, {
    ip_address = "localhost",
    port_number = 50051,
    connection = not_connected,
    
    connection_timer_ref
}).

-record(gobgp_server_t, {
    key,
    ip_address,
    port_number,
    pid
}).

%%%===================================================================
%%% API
%%%===================================================================

-define(call(A),gen_server:call(?MODULE, A)).
-define(cast(A),gen_server:cast(?MODULE, A)).

-define(dispatch_call(P, A),gen_server:call(pid(P), A)).
-define(dispatch_cast(P, A),gen_server:cast(pid(P), A)).

server(Op, Ip, Port) ->
    server({router,"localhost"}, Op, Ip, Port).
server({router, _} = RouterKey, Op, Ip, Port) ->
    ?call({server, RouterKey, Op, Ip, Port}).

router_id(Op, RouterId, AsNumber) ->
    router_id({router,"localhost"}, Op, RouterId, AsNumber).
router_id({router,_} = RouterKey, Op, RouterId, AsNumber) ->
    ?dispatch_call(RouterKey, {router_id, Op, RouterId, AsNumber}).

neighbor(Op, Ip, AsNumber) ->
    neighbor({router,"localhost"}, Op, Ip, AsNumber).
neighbor({router,_} = RouterKey, Op, Ip, AsNumber) ->
    ?dispatch_call(RouterKey, {neighbor, Op, Ip, AsNumber}).

route_reflector(_Op, _IpAddress) ->
    ok.
route_reflector({router,_}, _Op, _IpAddress) ->
    ok.

route(Op, RouteEntry) ->
    route({router,"localhost"}, Op, RouteEntry).
route({router,_} = RouterKey, Op, RouteEntry) ->
    ?dispatch_cast(RouterKey, {route, Op, RouteEntry}).

pid(RouterKey) ->
    ?call({get_pid, RouterKey}).

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
process_call({server, {router,Name} = RouterKey, set,Ip,PortNumer}= Request, State) ->
    case ets:lookup(?EtsConfig, RouterKey) of
        [] ->
            ?INFO("Request ~p", [Request]),
            {ok,Pid} = bgp_engine:start(Name, Ip, PortNumer),
            ets:insert(?EtsConfig, #gobgp_server_t{
                key = RouterKey,
                ip_address = Ip,
                port_number = PortNumer,
                pid = Pid
            });
        _ ->
            ok
    end,
    {reply, ok, State};
process_call({server, RouterKey, delete, _, _}, State) ->
    case ets:lookup(?EtsConfig, RouterKey) of
        [] ->
            ok;
        [#gobgp_server_t{pid = Pid}] ->
            bgp_engine:stop(Pid),
            ets:delete(?EtsConfig, RouterKey)
    end,
    {reply, ok, State};
process_call({get_pid, RouterKey}, State) ->
    [#gobgp_server_t{pid = Pid}]= ets:lookup(?EtsConfig, RouterKey),
    {reply, Pid, State};
process_call(Request, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({'EXIT',_,normal}, State) ->
    {noreply, State};
process_info_msg({init}, State) ->
    process_flag(trap_exit, true),
    ets:new(?EtsConfig,[named_table, {keypos, 2}, ordered_set, public]),
    {noreply, State};

process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.

%%%===================================================================
%%% Worker functions
%%%===================================================================
