%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2017 10:56 AM
%%%-------------------------------------------------------------------
-module(bgp_utils).
-author("vdasari").

%% API
-export([restart_timer/1, cancel_timer/1, pretty_print/1]).

cancel_timer(undefined) -> undefined;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref), undefined.
restart_timer(TimerRef) ->
    cancel_timer(TimerRef),
    erlang:send_after(5000, self(), retry_connection).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).
