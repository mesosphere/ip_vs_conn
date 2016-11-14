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

%% update the map with new connection data
-spec(update(#ip_vs_conn_state{}, conn_map()) -> conn_map()).
update(State, Map) -> update_conn(State, Map).

%% new connection
update_conn(#ip_vs_conn_state{tcp_state = TcpState, conn_state = ConnState, connection = Conn}, Acc) ->
    Status = #ip_vs_conn_status{conn_state = ConnState, tcp_state = TcpState},
    maps:put(Conn, Status, Acc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_conn_test_() ->
    Conn = <<"foobar">>,
    ConnState = #ip_vs_conn_state{connection = Conn, conn_state = <<"conn_state">>, tcp_state = established},
    New = update(ConnState, #{}),
    [?_assertEqual([Conn], maps:keys(New)),
     ?_assertEqual([#ip_vs_conn_status{conn_state = <<"conn_state">>, tcp_state = established}], maps:values(New))].

old_conn_test_() ->
    Conn = <<"foobar">>,
    Syn = #ip_vs_conn_state{connection = Conn, tcp_state = syn_recv, conn_state = <<"conn_state">>},
    Close = #ip_vs_conn_state{connection = Conn, tcp_state = close, conn_state = <<"state_conn">>},
    M0 = update(Syn, #{}),
    M1 = update(Close, M0),
    [#ip_vs_conn_status{conn_state = S0, tcp_state = syn_recv}] = maps:values(M0),
    [#ip_vs_conn_status{conn_state = S1, tcp_state = close}] = maps:values(M1),
    [?_assertEqual(<<"conn_state">>, S0),
     ?_assertEqual(<<"state_conn">>, S1)].

-endif.
