%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2015, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2016 04:36 PM
%%%-------------------------------------------------------------------
-module(ip_vs_conn_map).
-author("Anatoly Yakovenko").

-include("include/ip_vs_conn.hrl").

-export([update/3]).

%% update the map with new connection data
-spec(update(conn_map(), #ip_vs_conn_state{}, conn_map()) -> conn_map()).
update(Original, #ip_vs_conn_state{connection = Conn, tcp_state = State}, Updated) ->
    Current = maps:get(Conn, Original, badkey),
    update_conn(State, Conn, Updated, Current).

%% connection still in the same state
update_conn(State, Conn, Acc, Start = #ip_vs_conn_status{tcp_state = State}) ->
    maps:put(Conn, Start, Acc);

%% new connection or new state
update_conn(State, Conn, Acc, _Start) ->
    Now = erlang:monotonic_time(nano_seconds),
    Status = #ip_vs_conn_status{time_ns = Now, tcp_state = State},
    maps:put(Conn, Status, Acc).
