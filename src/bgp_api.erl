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
-include("bgp_api.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([server/1]).
-export([server/3, router_id/3, neighbor/4, route/2, status/0, api/2]).
-export([server/4, router_id/4, neighbor/5, route/3, status/1, api/3]).
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

-type router_key()  :: {router,atom()|iolist()}.

-spec server(Op::start|stop) -> {ok} | {error, Reason::term()}.
server(Op) ->
    server(Op, "localhost", 50051).
-spec server(Op::start|stop, Ip::ip_type(), Port::non_neg_integer()) ->
    {ok} | {error, Reason::term()}.
server(Op, Ip, Port) ->
    server({router,localhost}, Op, Ip, Port).
-spec server(RouterKey::router_key(), Op::start|stop, Ip::ip_type(), Port::non_neg_integer()) ->
    {ok} | {error, Reason::term()}.
server({router, _} = RouterKey, Op, Ip, Port) ->
    ?call({server, RouterKey, Op, Ip, Port}).

-spec router_id(Op::start|stop, RouterId::ip_type(), AsNumber::non_neg_integer()) ->
    {ok} | {error, Reason::term()}.
router_id(Op, RouterId, AsNumber) ->
    router_id({router,localhost}, Op, RouterId, AsNumber).
-spec router_id(RouterKey::router_key(), Op::start|stop, RouterId::ip_type(), AsNumber::non_neg_integer()) ->
    {ok} | {error, Reason::term()}.
router_id({router,_} = RouterKey, Op, RouterId, AsNumber) ->
    ?dispatch_call(RouterKey, {router_id, Op, RouterId, AsNumber}).

neighbor(Op, Ip, AsNumber, Family) ->
    neighbor({router,localhost}, Op, Ip, AsNumber, Family).
neighbor({router,_} = RouterKey, Op, Ip, AsNumber, Family) ->
    ?dispatch_call(RouterKey, {neighbor, Op, Ip, AsNumber, Family}).

route(Op, RouteEntry) ->
    route({router,localhost}, Op, RouteEntry).
route({router,_} = RouterKey, Op, RouteEntry) ->
    ?dispatch_cast(RouterKey, {route, Op, RouteEntry}).

-spec api(MethodName::atom, Request::tuple()) -> map().
api(MethodName, Request) ->
    api({router,localhost}, MethodName, Request).
-spec api(RouterKey::router_key(), MethodName::atom, Request::tuple()) -> map().
api({router,_} = RouterKey, MethodName, Request) ->
    ?dispatch_call(RouterKey, {api, MethodName, Request}).

status() ->
    status({router,localhost}).
status({router,_} = RouterKey) ->
    ?dispatch_call(RouterKey, status).

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
process_call({server, {router,Name} = RouterKey, start,Ip,PortNumer}= Request, State) ->
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
process_call({server, RouterKey, stop, _, _}, State) ->
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
