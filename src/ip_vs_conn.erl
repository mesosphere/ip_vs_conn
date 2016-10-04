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

-spec(parse(string()) -> list(#ip_vs_conn{})).

parse(Filepath) -> file_fold(fun parse/2, [], file:open(Filepath, [read])).

file_fold(Func, Z, {ok, Fd}) -> file_fold_line(Func, Z, Fd, file:read_line(Fd));
file_fold(_, Z, _) -> Z.

file_fold_line(Func, Z, Fd, {ok, Data}) -> file_fold(Func, Func(Data, Z), {ok, Fd});
file_fold_line(_, Z, _, _) -> Z.

%% matched first line
parse(Data, Conns) ->
    Tokens = string:tokens(string:strip(Data, both, $\n), " "),
    Conn = to_connection(Tokens),
    Conn ++ Conns.

to_connection(["Pro", "FromIP", "FPrt", "ToIP", "TPrt", "DestIP",
               "DPrt", "State", "Expires", "PEName", "PEData"]) -> [];

to_connection(Ls = [_Pro, _FromIP, _FPrt, _ToIP, _TPrt,
                    _DestIP, _DPrt, _State, _Expires]) ->
    to_connection(Ls ++ ["",""]);

to_connection([Pro, FromIP, FPrt, ToIP, TPrt,
               DestIP, DPrt, State, Expires, PEName, PEData]) ->
    [#ip_vs_conn{ protocol =  to_protocol(Pro),
                  from_ip = hex_str_to_int(FromIP),
                  from_port = hex_str_to_int(FPrt),
                  to_ip = hex_str_to_int(ToIP),
                  to_port = hex_str_to_int(TPrt),
                  dst_ip = hex_str_to_int(DestIP),
                  dst_port = hex_str_to_int(DPrt),
                  tcp_state = to_tcp_state(State),
                  expires = dec_str_to_int(Expires),
                  pe_name = PEName,
                  pe_data = PEData
                }].

to_protocol("TCP") -> tcp;
to_protocol("UDP") -> udp.

to_tcp_state("SYN_RECV") -> syn_recv;
to_tcp_state("FIN_WAIT") -> fin_wait;
to_tcp_state("TIME_WAIT") -> time_wait;
to_tcp_state("ESTABLISHED") -> established.

hex_str_to_int(Str) -> erlang:list_to_integer(Str, 16).
dec_str_to_int(Str) -> erlang:list_to_integer(Str, 10).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_first_line_test_() -> 
    Str="Pro FromIP   FPrt ToIP     TPrt DestIP   DPrt State       Expires PEName PEData",
    [?_assertEqual([], parse(Str, []))].

parse_conn_line_test_() -> 
    Str="TCP 0A004FB6 0045 0A004FB6 1F90 0A004FB6 1F91 SYN_RECV         57",
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

to_protocol_test_() ->
    [?_assertEqual(tcp, to_protocol("TCP")),
     ?_assertEqual(udp, to_protocol("UDP"))].

to_tcp_state_test_() ->
    [?_assertEqual(syn_recv, to_tcp_state("SYN_RECV")),
     ?_assertEqual(established, to_tcp_state("ESTABLISHED")),
     ?_assertEqual(time_wait, to_tcp_state("TIME_WAIT")),
     ?_assertEqual(fin_wait, to_tcp_state("FIN_WAIT"))].

hex_str_to_int_test_() ->
    [?_assertEqual(8081, hex_str_to_int("1f91")),
     ?_assertEqual(8080, hex_str_to_int("1f90"))].

dec_str_to_int_test_() ->
    [?_assertEqual(57, dec_str_to_int("57")),
     ?_assertEqual(58, dec_str_to_int("58"))].

-endif.
