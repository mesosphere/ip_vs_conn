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

-type connection() :: binary().
-type conn_state() :: binary().

-record(ip_vs_conn_state, {
    connection  :: connection(),
    conn_state  :: conn_state(),
    tcp_state   :: tcp_state()
    }).

-record(ip_vs_conn, {
    protocol    :: protocol(),
    tcp_state   :: tcp_state(),
    from_ip     :: integer(),
    from_port   :: integer(),
    to_ip       :: integer(),
    to_port     :: integer(),
    dst_ip      :: integer(),
    dst_port    :: integer(),
    expires    :: integer() | 9999
    }).

-type monotonic_time_ns() :: integer().

-record(ip_vs_conn_status, {
    conn_state  :: conn_state(),
    tcp_state   :: tcp_state()
    }).

-type conn_map() :: #{ connection() => #ip_vs_conn_status{} }.
