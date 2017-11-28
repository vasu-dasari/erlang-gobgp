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

-include("bgp_api.hrl").

%% API
-export([restart_timer/1, cancel_timer/1, pretty_print/1, rdrt2binary/1, record_to_proplist/1, record_to_proplist/2]).

cancel_timer(undefined) -> undefined;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref), undefined.
restart_timer(TimerRef) ->
    cancel_timer(TimerRef),
    erlang:send_after(5000, self(), retry_connection).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).

rdrt2binary(Str) ->
    [A, B] = [binary:encode_unsigned(list_to_integer(V)) || V <- string:tokens(Str, ":")],

    B_Bin = case size(B) of
        1 -> <<0,0,0, B/binary>>;
        2 -> <<0,0, B/binary>>;
        3 -> <<0, B/binary>>;
        4 -> B
    end,
    A_Bin = case size(A) of
        1 -> <<0,A/binary>>;
        2 -> A
    end,
    <<0,0,A_Bin/binary, B_Bin/binary>>.

-define(R2P(Record),
    record_to_proplist(#Record{} = Rec) ->
        List = [record_to_proplist(R) || R <- tuple_to_list(Rec)],
        ElemList = [{record, Record}] ++ lists:zip(record_info(fields, Record), tl(List)),
        PropList = [{K,V} || {K,V} <- ElemList, (V /= undefined) andalso (V /= []) andalso (V /= <<>>)],
        case PropList of
            [{record, _}] ->
                [];
            _ ->
                PropList
        end
).

record_to_proplist(to_str, R) ->
    pretty_print(record_to_proplist(R)).

record_to_proplist({}) -> [];
?R2P(route_entry_t);
?R2P(neighbor_advt_t);
?R2P(path_advt_t);
?R2P(route_advt_t);
record_to_proplist(List) when is_list(List) ->
    lists:foldr(fun
        (Entry, Acc) ->
            [record_to_proplist(Entry) | Acc]
    end, [], List);
record_to_proplist(Rec) -> Rec.

