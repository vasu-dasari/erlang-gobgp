%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2017 11:52 PM
%%%-------------------------------------------------------------------
-author("vdasari").

-record(route_entry_t, {
    type            :: macadv,
    ip_address      :: non_neg_integer() | binary() | iolist(),
    mac_address     :: binary() | iolist(),
    service_id      :: non_neg_integer(),
    rd              :: binary() | iolist(),
    rt              :: binary() | iolist(),
    encap           :: mim | vxlan
}).