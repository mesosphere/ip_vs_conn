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
-spec(update(maps:map() -> #ip_vs_conn_state{} -> maps:new()) -> maps:new()).
update(Original, Conn, Updated) ->
    Current = maps:get(Conn, Original, badkey),
    update_conn(Conn, Updated, Current).

%% new connection
update_conn(#ip_vs_conn_state{connection = Conn, tcp_state = syn_recv}, Acc, badkey) ->
    maps:put(Conn, erlang:monotonic_time(seconds), Acc);

%% skip connections in the other state
update_conn(_Conn, Acc, badkey) -> Acc;

%% old connection still in syn_recv state
update_conn(#ip_vs_conn_state{connection = Conn, tcp_state = syn_recv}, Acc, Start) ->
    maps:put(Conn, Start, Acc).
