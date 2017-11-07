%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 10:00 PM
%%%-------------------------------------------------------------------
-module(gobgp_nif).
-author("vdasari").

%% API
-export([route_family/1, reload_nif/0]).
-export([command/1, init/0]).
-on_load(init/0).

-include("logger.hrl").

-define(APPNAME, gobgp).
-define(LIBNAME, gobgp_nif).

%% NIF Calls
route_family(_) -> not_loaded(?LINE).
command(_) -> not_loaded(?LINE).

init() ->
    {ok,LibFile} = application:get_env(?APPNAME, libgobgp),
    case filelib:is_file(LibFile) of
        true ->
            ok;
        _ ->
            exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
    end,
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,

    erlang:load_nif(SoName, LibFile).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

reload_nif() ->
    code:purge(?MODULE),
    code:delete(?MODULE),
    code:purge(?MODULE),
    code:delete(?MODULE),
    {module,?MODULE} == code:load_file(?MODULE).