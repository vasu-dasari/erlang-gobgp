%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2017 11:52 PM
%%%-------------------------------------------------------------------
-author("vdasari").

-type ip_type()     :: binary() | non_neg_integer() | iolist().
-type mac_type()    :: binary() | iolist().
-type route_type()  :: macadv | multicast.
-type oper_type()   :: add | delete.
-type bgp_state()   :: idle | connect | active | open_sent | open_confirm | estbl.
-type admin_state() :: up | down.

-record(route_entry_t, {
    type            :: route_type(),
    family          :: gobgp_pb:'Family'(),
    asn             :: non_neg_integer(),
    nexthop         :: ip_type(),
    ip_address      :: ip_type(),
    mac_address     :: mac_type(),
    service_id      :: non_neg_integer(),
    rd              :: binary() | iolist(),
    rt              :: binary() | iolist(),
    encap           :: mim | vxlan
}).

-record(neighbor_advt_t, {
    neighbor_ip     :: ip_type(),
    admin_state     :: admin_state(),
    oper_state      :: bgp_state()
}).

-record(path_advt_t, {
    oper        :: oper_type(),
    asn         :: non_neg_integer(),
    neighbor_ip :: ip_type()
}).

-record(route_advt_t, {
    type        :: route_type(),
    rd          :: iolist(),
    etag        :: non_neg_integer(),
    ip          :: ip_type(),
    mac         :: mac_type(),
    paths       :: [#path_advt_t{}]
}).