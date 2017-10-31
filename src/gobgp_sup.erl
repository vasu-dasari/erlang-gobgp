%%%-------------------------------------------------------------------
%% @doc gobgp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gobgp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(Process(Name, Type),
    {Name, {Name, start_link, []}, permanent, 2000, Type, [Name]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    _Restart = permanent,
    _Shutdown = 2000,
    _Type = worker,

    {ok, {SupFlags, [
        ?Process(bgp_api, worker)
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================