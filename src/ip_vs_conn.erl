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
-export([parse/1
        ]).

%% Parse the proc file for connections in SYN_RECV state
-spec(parse(string()) -> list(#ip_vs_conn{})).
parse(Filepath) -> file_fold(fun parse/2, [], file:open(Filepath, [read, binary, raw, {read_ahead, 1024*64}])).

file_fold(Func, Z, {ok, Fd}) -> file_fold_line(Func, Z, Fd, file:read_line(Fd));
file_fold(_, Z, _) -> Z.

file_fold_line(Func, Z, Fd, {ok, Data}) -> file_fold(Func, Func(Data, Z), {ok, Fd});
file_fold_line(_, Z, _, _) -> Z.

parse(Data, Conns) ->
    Conn = syn_recv(Data),
    Conn ++ Conns.

%% matched data
syn_recv(<<_Start:46/bytes,"SYN_RECV",_Rest/binary>> = Data) -> make_conn(Data);
syn_recv(_) -> [].

make_conn(<<Pro:3/bytes,$\s,
            FromIP:8/bytes,$\s,
            FPrt:4/bytes,$\s,
            ToIP:8/bytes,$\s,
            TPrt:4/bytes,$\s,
            DestIP:8/bytes,$\s,
            DPrt:4/bytes,$\s,
            Status:11/bytes,$\s,$\s,$\s,$\s,$\s,$\s,
            Expires:2/bytes, PE/binary>>) ->
    [#ip_vs_conn{ protocol =  to_protocol(Pro),
                  from_ip = hex_str_to_int(FromIP),
                  from_port = hex_str_to_int(FPrt),
                  to_ip = hex_str_to_int(ToIP),
                  to_port = hex_str_to_int(TPrt),
                  dst_ip = hex_str_to_int(DestIP),
                  dst_port = hex_str_to_int(DPrt),
                  tcp_state = to_tcp_state(Status),
                  expires = to_expires(Expires),
                  pe_name = to_pe_name(PE),
                  pe_data = to_pe_data(PE)
                }].

to_protocol(<<"TCP">>) -> tcp;
to_protocol(<<"UDP">>) -> udp.

to_tcp_state(<<"SYN_RECV   ">>) -> syn_recv;
to_tcp_state(<<"NONE       ">>) -> none;
to_tcp_state(<<"ESTABLISHED">>) -> established;
to_tcp_state(<<"SYN_SENT   ">>) -> syn_sent;
to_tcp_state(<<"FIN_WAIT   ">>) -> fin_wait;
to_tcp_state(<<"TIME_WAIT  ">>) -> time_wait;
to_tcp_state(<<"CLOSE      ">>) -> close;
to_tcp_state(<<"CLOSE_WAIT ">>) -> close_wait;
to_tcp_state(<<"LAST_ACK   ">>) -> last_ack;
to_tcp_state(<<"LISTEN     ">>) -> listen;
to_tcp_state(<<"SYNACK     ">>) -> synack.

to_expires(<<$\s,Exp:1/bytes>>) -> erlang:binary_to_integer(Exp, 10);
to_expires(<<Exp:2/bytes>>) -> erlang:binary_to_integer(Exp, 10).

to_pe_name(Bin) -> lists:nth(1, to_pe(Bin)).
to_pe_data(Bin) -> lists:nth(2, to_pe(Bin)).

to_pe(<<"\n">>) -> ["",""];
to_pe(Bin) -> string:tokens(erlang:binary_to_list(Bin), " \n").

hex_str_to_int(Str) -> erlang:binary_to_integer(Str, 16).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_first_line_test_() ->
    Str = <<"Pro FromIP   FPrt ToIP     TPrt DestIP   DPrt State       Expires PEName PEData\n">>,
    [?_assertEqual([], parse(Str, []))].

parse_conn_line_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV         57\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        tcp_state = syn_recv,
                        expires = 57,
                        pe_name = "",
                        pe_data = ""
                      },
    [?_assertEqual([Conn], parse(Str, []))].

parse_conn_line2_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        tcp_state = syn_recv,
                        expires = 7,
                        pe_name = "",
                        pe_data = ""
                      },
    [?_assertEqual([Conn], parse(Str, []))].

parse_conn_line3_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081,
                        tcp_state = syn_recv,
                        expires = 7,
                        pe_name = "foo",
                        pe_data = "bar"
                      },
    [?_assertEqual([Conn], parse(Str, []))].


to_protocol_test_() ->
    [?_assertEqual(tcp, to_protocol(<<"TCP">>)),
     ?_assertEqual(udp, to_protocol(<<"UDP">>))].

to_tcp_state_test_() ->
    [?_assertEqual(none,       to_tcp_state(<<"NONE       ">>)),
     ?_assertEqual(established,to_tcp_state(<<"ESTABLISHED">>)),
     ?_assertEqual(syn_sent,   to_tcp_state(<<"SYN_SENT   ">>)),
     ?_assertEqual(syn_recv,   to_tcp_state(<<"SYN_RECV   ">>)),
     ?_assertEqual(fin_wait,   to_tcp_state(<<"FIN_WAIT   ">>)),
     ?_assertEqual(time_wait,  to_tcp_state(<<"TIME_WAIT  ">>)),
     ?_assertEqual(close,      to_tcp_state(<<"CLOSE      ">>)),
     ?_assertEqual(close_wait, to_tcp_state(<<"CLOSE_WAIT ">>)),
     ?_assertEqual(last_ack,   to_tcp_state(<<"LAST_ACK   ">>)),
     ?_assertEqual(listen,     to_tcp_state(<<"LISTEN     ">>)),
     ?_assertEqual(synack,     to_tcp_state(<<"SYNACK     ">>))].


hex_str_to_int_test_() ->
    [?_assertEqual(8081, hex_str_to_int(<<"1f91">>)),
     ?_assertEqual(8080, hex_str_to_int(<<"1f90">>))].

dec_str_to_int_test_() ->
    [?_assertEqual(57, to_expires(<<"57">>)),
     ?_assertEqual(8, to_expires(<<" 8">>))].

-endif.

