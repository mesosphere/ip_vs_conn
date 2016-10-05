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

-export([update/2]).

update(Map, Conns) ->
    NewMap = lists:foldl(fun(Conn, Acc) ->
                              update_map(Map, Conn, Acc)
                          end,
                          maps:new(), Conns),
    NewMap.

update_map(Syns, Conn, Acc) ->
    Current = maps:get(tcp_conn(Conn), Syns, badkey),
    update_conn(Conn, Acc, Current).

%% new connection
update_conn(Conn = #ip_vs_conn{tcp_state = syn_recv}, Acc, badkey) ->
    maps:put(tcp_conn(Conn), erlang:monotonic_time(seconds), Acc);

%% skip connections in the other state
update_conn(_Conn, Acc, badkey) -> Acc;

%% old connection still in syn_recv state
update_conn(Conn = #ip_vs_conn{tcp_state = syn_recv}, Acc, Start) ->
    maps:put(tcp_conn(Conn), Start, Acc).

tcp_conn(Conn) ->
    #tcp_conn{from_ip   = Conn#ip_vs_conn.from_ip,
              from_port = Conn#ip_vs_conn.from_port,
              to_ip     = Conn#ip_vs_conn.to_ip,
              to_port   = Conn#ip_vs_conn.to_port,
              dst_ip    = Conn#ip_vs_conn.dst_ip,
              dst_port  = Conn#ip_vs_conn.dst_port}.
