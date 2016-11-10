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

%% old connection
update_conn(State, Conn, Acc, #ip_vs_conn_status{time_ns = Time}) ->
    Status = #ip_vs_conn_status{time_ns = Time, tcp_state = State},
    maps:put(Conn, Status, Acc);

%% new connection
update_conn(State, Conn, Acc, badkey) ->
    Now = erlang:monotonic_time(nano_seconds),
    Status = #ip_vs_conn_status{time_ns = Now, tcp_state = State},
    maps:put(Conn, Status, Acc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_conn_test_() ->
    Old = #{},
    Conn = <<"foobar">>,
    ConnState = #ip_vs_conn_state{connection = Conn, tcp_state = established},
    New = update(Old, ConnState, #{}),
    [?_assertEqual([Conn], maps:keys(New)),
     ?_assertMatch([#ip_vs_conn_status{time_ns = _, tcp_state = established}], maps:values(New))].

old_conn_test_() ->
    Conn = <<"foobar">>,
    Syn = #ip_vs_conn_state{connection = Conn, tcp_state = syn_recv},
    Close = #ip_vs_conn_state{connection = Conn, tcp_state = close},
    M0 = update(#{}, Syn, #{}),
    M1 = update(M0, Close, #{}),
    [#ip_vs_conn_status{time_ns = T0, tcp_state = syn_recv}] = maps:values(M0),
    [#ip_vs_conn_status{time_ns = T1, tcp_state = close}] = maps:values(M1),
    [?_assertEqual(T1, T0)].

-endif.
