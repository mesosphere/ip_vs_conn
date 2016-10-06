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

%% Fold over the proc file for connections in SYN_RECV state
-spec(fold(fun((#ip_vs_conn_state{}, term()) -> term()), term(), string()) -> term()).
fold(Func, ZZ, Filepath) -> 
    file_fold(
      fun(Line, Acc) -> syn_recv_line(Func, Line, Acc) end, ZZ, 
      file:open(Filepath, [read, binary, raw, {read_ahead, 1024*64}])).

%% parse connection data 
-spec(parse(binary()) -> #ip_vs_conn{}).
parse(<<Pro:3/bytes,$\s,
        FromIP:8/bytes,$\s,
        FPrt:4/bytes,$\s,
        ToIP:8/bytes,$\s,
        TPrt:4/bytes,$\s,
        DestIP:8/bytes,$\s,
        DPrt:4/bytes,$\s>>) ->
    #ip_vs_conn{ protocol =  to_protocol(Pro),
                 from_ip = hex_str_to_int(FromIP),
                 from_port = hex_str_to_int(FPrt),
                 to_ip = hex_str_to_int(ToIP),
                 to_port = hex_str_to_int(TPrt),
                 dst_ip = hex_str_to_int(DestIP),
                 dst_port = hex_str_to_int(DPrt)}.


file_fold(Func, Z, {ok, Fd}) -> file_fold_line(Func, Z, Fd, file:read_line(Fd));
file_fold(_, Z, _) -> Z.

file_fold_line(Func, Z, Fd, {ok, Data}) -> file_fold(Func, Func(Data, Z), {ok, Fd});
file_fold_line(_, Z, _, _) -> Z.

syn_recv_line(Func, Line, ZZ) ->
    case syn_recv(Line) of
        [Syn] -> Func(Syn, ZZ);
        _ -> ZZ
    end.

%% matched data
syn_recv(<<Connection:46/bytes,"SYN_RECV",_Rest/binary>>) -> 
    [#ip_vs_conn_state{ connection = Connection,
                        tcp_state = syn_recv}];
syn_recv(_) -> [].

to_protocol(<<"TCP">>) -> tcp;
to_protocol(<<"UDP">>) -> udp.

hex_str_to_int(Str) -> erlang:binary_to_integer(Str, 16).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_first_line_test_() ->
    Str = <<"Pro FromIP   FPrt ToIP     TPrt DestIP   DPrt State       Expires PEName PEData\n">>,
    [?_assertEqual([], syn_recv(Str))].

parse_conn_line_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV         57\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081
                      },
    [{ip_vs_conn_state, BConn, syn_recv}] = syn_recv(Str),
    [?_assertEqual(Conn, parse(BConn))].
    

parse_conn_line2_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081
                      },
    [{ip_vs_conn_state, BConn, syn_recv}] = syn_recv(Str),
    [?_assertEqual(Conn, parse(BConn))].

parse_conn_line3_test_() ->
    Str = <<"TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV          7 foo bar\n">>,
    Conn = #ip_vs_conn{ protocol =  tcp,
                        from_ip = 167792566,
                        from_port = 69,
                        to_ip = 167792566,
                        to_port = 8080,
                        dst_ip = 167792566,
                        dst_port = 8081
                      },
    [{ip_vs_conn_state, BConn, syn_recv}] = syn_recv(Str),
    [?_assertEqual(Conn, parse(BConn))].


to_protocol_test_() ->
    [?_assertEqual(tcp, to_protocol(<<"TCP">>)),
     ?_assertEqual(udp, to_protocol(<<"UDP">>))].

hex_str_to_int_test_() ->
    [?_assertEqual(8081, hex_str_to_int(<<"1f91">>)),
     ?_assertEqual(8080, hex_str_to_int(<<"1f90">>))].

-endif.

