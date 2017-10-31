%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Nov 2016 11:20 AM
%%%-------------------------------------------------------------------
-author("vdasari").

%%------------------------------------------------------------------------------
%% Logging macros
%%------------------------------------------------------------------------------

-define(DEBUG(Msg),
    lager:debug(Msg)).
-define(DEBUG(Msg, Args),
    lager:debug(Msg, Args)).

-define(INFO(Msg),
    lager:info([{linc, x}],Msg)).
-define(INFO(Msg, Args),
    lager:info([{linc, x}],Msg, Args)).

-define(WARNING(Msg),
    lager:warning([{linc, x}],Msg)).
-define(WARNING(Msg, Args),
    lager:warning([{linc, x}],Msg, Args)).

-define(ERROR(Msg),
    lager:error([{linc, x}],Msg)).
-define(ERROR(Msg, Args),
    lager:error([{linc, x}],Msg, Args)).
