%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2016, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2016 11:07 AM
%%%-------------------------------------------------------------------
-module(ip_vs_conn).
-include_lib("include/ip_vs_conn.hrl").
-export([fold/3,
         parse/1
        ]).

%% Fold over the proc file for connections
%% only parses SYN_RECV and ESTABLISHED connections
-spec(fold(fun((#ip_vs_conn_state{}, term()) -> term()), term(), string()) -> term()).
fold(Func, ZZ, Filepath) -> 
    file_fold(
      fun(Line, Acc) -> recv_line(Func, Line, Acc) end, ZZ, 
      file:open(Filepath, [read, binary, raw, {read_ahead, 1024*64}])).

%% parse connection data 
-spec(parse(#ip_vs_conn_state{} | {connection(), #ip_vs_conn_status{}}) -> #ip_vs_conn{}).
parse(#ip_vs_conn_state{
         connection = <<Pro:3/bytes,$\s,
                        FromIP:8/bytes,$\s,
                        FPrt:4/bytes,$\s,
                        ToIP:8/bytes,$\s,
                        TPrt:4/bytes,$\s,
                        DestIP:8/bytes,$\s,
                        DPrt:4/bytes,$\s>>,
          conn_state = Expires
      }) ->
    #ip_vs_conn{ protocol =  to_protocol(Pro),
                 from_ip = hex_str_to_int(FromIP),
                 from_port = hex_str_to_int(FPrt),
                 to_ip = hex_str_to_int(ToIP),
                 to_port = hex_str_to_int(TPrt),
                 dst_ip = hex_str_to_int(DestIP),
                 dst_port = hex_str_to_int(DPrt),
                 expires = expires(Expires)};

parse({Conn,  #ip_vs_conn_status{ tcp_state = TcpState, conn_state = ConnState}}) ->
    parse(#ip_vs_conn_state{connection = Conn, conn_state = ConnState, tcp_state = TcpState}).

file_fold(Func, Z, {ok, Fd}) -> file_fold_line(Func, Z, Fd, file:read_line(Fd));
file_fold(_, Z, _) -> Z.

file_fold_line(Func, Z, Fd, {ok, Data}) -> file_fold(Func, Func(Data, Z), {ok, Fd});
file_fold_line(_, Z, _, _) -> Z.

recv_line(Func, Line, ZZ) ->
    case recv(Line) of
        [Syn] -> Func(Syn, ZZ);
        _ -> ZZ
    end.

%% matched data
recv(<<Connection:46/bytes,"ESTABLISHED",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = established}];
recv(<<Connection:46/bytes,"SYN_SENT",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = syn_sent}];
recv(<<Connection:46/bytes,"SYN_RECV",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = syn_recv}];
recv(<<Connection:46/bytes,"FIN_WAIT",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = fin_wait}];
recv(<<Connection:46/bytes,"TIME_WAIT",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = time_wait}];
recv(<<Connection:46/bytes,"CLOSE_WAIT",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = close_wait}];
recv(<<Connection:46/bytes,"CLOSE",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = close}];
recv(<<Connection:46/bytes,"LAST_ACK",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = last_ack}];
recv(<<Connection:46/bytes,"LISTEN",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = listen}];
recv(<<Connection:46/bytes,"SYNACK",Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        conn_state = Rest,
                        tcp_state = synack}];
recv(_) -> [].

to_protocol(<<"TCP">>) -> tcp;
to_protocol(<<"UDP">>) -> udp.

hex_str_to_int(Str) -> erlang:binary_to_integer(Str, 16).

expires(<<$\s,Rest/binary>>) -> expires(Rest);
expires(<<Expires:1/bytes,$\s,_Rest/binary>>) ->    erlang:binary_to_integer(Expires, 10);
expires(<<Expires:1/bytes,$\n>>) ->                 erlang:binary_to_integer(Expires, 10);
expires(<<Expires:2/bytes,$\s,_Rest/binary>>) ->    erlang:binary_to_integer(Expires, 10);
expires(<<Expires:2/bytes,$\n>>) ->                 erlang:binary_to_integer(Expires, 10);
expires(<<Expires:3/bytes,$\s,_Rest/binary>>) ->    erlang:binary_to_integer(Expires, 10);
expires(<<Expires:3/bytes,$\n>>) ->                 erlang:binary_to_integer(Expires, 10);
expires(_) -> 9999.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
expires_test_() ->
    [?_assertEqual(9999, expires(<<>>)),
     ?_assertEqual(9999, expires(<<"\n">>)),
     ?_assertEqual(9999, expires(<<" ">>)),
     ?_assertEqual(1, expires(<<" 1\n">>)),
     ?_assertEqual(1, expires(<<" 1 ">>)),
     ?_assertEqual(11, expires(<<" 11\n">>)),
     ?_assertEqual(11, expires(<<" 11 ">>)),
     ?_assertEqual(111, expires(<<" 111\n">>)),
     ?_assertEqual(111, expires(<<" 111 ">>)),
     ?_assertEqual(111, expires(<<"      111    ">>)),
     ?_assertEqual(111, expires(<<"      111    fdsafadsfs\n">>))
    ].

parse_first_line_test_() ->
    Str = <<"Pro FromIP   FPrt ToIP     TPrt DestIP   DPrt State       Expires PEName PEData\n">>,
    [?_assertEqual([], recv(Str))].

parse_conn_line_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV         57\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 57
                      },
    [BConn = #ip_vs_conn_state{tcp_state = syn_recv}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].
    
parse_conn_line2_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 7
                      },
    [BConn = #ip_vs_conn_state{tcp_state = syn_recv}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_established2_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 ESTABLISHED     997\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 997
                      },
    [BConn = #ip_vs_conn_state{tcp_state = established}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].


parse_conn_line3_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 7
                      },
    [BConn = #ip_vs_conn_state{tcp_state = syn_recv}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_established4_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 ESTABLISHED       7 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 7
                      },
    [BConn = #ip_vs_conn_state{tcp_state = established}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_close_wait_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 CLOSE_WAIT      999 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 999
                      },
    [BConn = #ip_vs_conn_state{tcp_state = close_wait}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_last_ack_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 LAST_ACK        999 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 999
                      },
    [BConn = #ip_vs_conn_state{tcp_state = last_ack}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_listen_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 LISTEN          999 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 999
                      },
    [BConn = #ip_vs_conn_state{tcp_state = listen}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_synack_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYNACK          999 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 999
                      },
    [BConn = #ip_vs_conn_state{tcp_state = synack}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_syn_sent_test() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_SENT        999 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        expires = 999
                      },
    [BConn = #ip_vs_conn_state{tcp_state = syn_sent}] = recv(Str),
    [?_assertEqual(Conn, parse(BConn))].



to_protocol_test_() ->
    [?_assertEqual(tcp, to_protocol(<<"TCP">>)),
     ?_assertEqual(udp, to_protocol(<<"UDP">>))].

hex_str_to_int_test_() ->
    [?_assertEqual(8081, hex_str_to_int(<<"1f91">>)),
     ?_assertEqual(8080, hex_str_to_int(<<"1f90">>))].

-endif.

