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
-include_lib("eunit/include/eunit.hrl").
-export([parse/1
        ]).

-spec(parse(string()) -> list(#ip_vs_conn{})).

parse(Filepath) ->
  {ok, Fd} = file:open(Filepath),
  {ok, Line} = file:read_line(Fd),
  _Tokens = string:tokens(Line, " "),
  parse(Fd, file:read_line(Fd), []).

%% matched first line
parse(Fd, {ok, Data}, Conns) -> 
  Tokens = string:tokens(Data, " "),
  Conn = to_connection(Tokens),
  parse(Fd, file:read_line(Fd), [Conn | Conns]);

parse(_, _, Conns) -> Conns.

to_connection(Ls = [_Pro, _FromIP, _FPrt, _ToIP, _TPrt,
                    _DestIP, _DPrt, _State, _Expires]) ->
  to_connection(Ls ++ ["",""]);

to_connection([Pro, FromIP, FPrt, ToIP, TPrt,
               DestIP, DPrt, State, Expires, PEName, PEData]) ->
  #ip_vs_conn{ protocol =  to_protocol(Pro),
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
             }.

to_protocol("TCP") -> tcp;
to_protocol("UDP") -> udp.

to_tcp_state("SYN_RECV") -> syn_recv;
to_tcp_state("ESTABLISHED") -> established.

hex_str_to_int(Str) -> erlang:list_to_integer(Str, 16).
dec_str_to_int(Str) -> erlang:list_to_integer(Str, 10).

-ifdef(TEST).
parse_test_() -> 
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
  [?_assertEqual([Conn], parse(0, {ok, Str}, []))].



to_protocol_test_() -> [?_assertEqual(tcp, to_protocol("TCP")),
                        ?_assertEqual(udp, to_protocol("UDP"))].

to_tcp_state_test_() -> [?_assertEqual(syn_recv, to_tcp_state("SYN_RECV")),
                         ?_assertEqual(established, to_tcp_state("ESTABLISHED"))].

hex_str_to_int_test_() -> [?_assertEqual(8081, hex_str_to_int("1f91")),
                           ?_assertEqual(8080, hex_str_to_int("1f90"))].

dec_str_to_int_test_() -> [?_assertEqual(57, dec_str_to_int("57")),
                           ?_assertEqual(58, dec_str_to_int("58"))].

-endif.
