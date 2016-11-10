%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2016, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2016 11:07 AM
%%%-------------------------------------------------------------------

-type protocol() :: tcp | udp.
-type tcp_state() :: none | established | syn_sent | syn_recv | fin_wait | time_wait | close | close_wait | last_ack | listen | synack.

-record(ip_vs_conn_state, {
    connection  :: binary(),
    tcp_state   :: tcp_state()
    }).

-record(ip_vs_conn, {
    protocol    :: protocol(),
    from_ip     :: integer(),
    from_port   :: integer(),
    to_ip       :: integer(),
    to_port     :: integer(),
    dst_ip      :: integer(),
    dst_port    :: integer()
    }).

-type monotonic_time_ns() :: integer().

-record(ip_vs_conn_status, {
    time_ns     :: monotonic_time_ns(),
    tcp_state   :: tcp_state()
    }).

-type conn_map() :: #{ binary() => #ip_vs_conn_status{} }.
