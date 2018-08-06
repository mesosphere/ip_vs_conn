-module(ip_vs_conn_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("include/ip_vs_conn.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,

    test_gen_server/1,
    test_parse/1,
    test_parse_missing/1,
    test_server/1,
    test_server2/1,
    test_server_wait/1,
    test_parse_large_close/1,
    test_parse_large_syn_recv/1,
    test_update/1
]).

all() -> [test_gen_server,
          test_parse,
          test_parse_missing,
          test_server,
          test_server2,
          test_server_wait,
          test_parse_large_close,
          test_parse_large_syn_recv,
          test_update
         ].

parse(Conn, List) -> 
    [ip_vs_conn:parse(Conn) | List].

test_parse(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Ret = [{ip_vs_conn,tcp,syn_recv,167792566,47808,167792566,8080,167792566,8081, 59},
           {ip_vs_conn,tcp,syn_recv,167792566,62061,167792566,8080,167792566,8081, 58},
           {ip_vs_conn,tcp,syn_recv,167792566,69,167792566,8080,167792566,8081, 57}],
    Ret = ip_vs_conn:fold(fun parse/2, [], Proc),
    ok.

test_parse_large_close(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Start = erlang:monotonic_time(micro_seconds),
    Ret = ip_vs_conn:fold(fun parse/2, [], Proc),
    End = erlang:monotonic_time(micro_seconds),
    ct:pal("time to parse ~p", [End - Start]),
    65535 = length(Ret),
    ok.

test_parse_large_syn_recv(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Start = erlang:monotonic_time(micro_seconds),
    Ret = ip_vs_conn:fold(fun parse/2, [], Proc),
    End = erlang:monotonic_time(micro_seconds),
    ct:pal("time to parse ~p", [End - Start]),
    65535 = length(Ret),
    ok.

test_parse_missing(_Config) ->
    [] = ip_vs_conn:fold(fun parse/2, [], "foobar"),
    ok.

test_gen_server(_Config) ->
    erlang:send(ip_vs_conn_monitor, hello),
    ok = gen_server:call(ip_vs_conn_monitor, hello),
    ok = gen_server:cast(ip_vs_conn_monitor, hello),
    sys:suspend(ip_vs_conn_monitor),
    sys:change_code(ip_vs_conn_monitor, random_old_vsn, ip_vs_conn_monitor, []),
    sys:resume(ip_vs_conn_monitor).

get_connections() ->
    ok = ip_vs_conn_monitor:get_connections(new_connections),
    receive
        {new_connections, New} ->
            ct:pal("got ~p", [New]),
            {ok, New}
    end.

test_server(_Config) ->
    {ok, Map} = get_connections(),
    Keys = lists:sort(
             [{ip_vs_conn, tcp, syn_recv, 167792566,47808,167792566,8080,167792566,8081, 59},
              {ip_vs_conn, tcp, syn_recv, 167792566,62061,167792566,8080,167792566,8081, 58},
              {ip_vs_conn, tcp, syn_recv, 167792566,69,167792566,8080,167792566,8081, 57}]),
    Keys = lists:sort(lists:map(fun ip_vs_conn:parse/1, maps:to_list(Map))),
    ok.

test_server2(_Config) ->
    {ok, Map} = get_connections(),
    Keys = [{ip_vs_conn, tcp, syn_recv, 167792566, 69, 167792566, 8080, 167792566, 8081, 57},
            {ip_vs_conn, tcp, fin_wait, 167792566, 47808, 167792566, 8080, 167792566, 8081, 59},
            {ip_vs_conn, tcp, time_wait, 167792566, 47809, 167792566, 8080, 167792566, 8081, 59},
            {ip_vs_conn, tcp, established, 167792566, 62061, 167792566, 8080, 167792566, 8081, 58}],
    Keys = lists:map(fun ip_vs_conn:parse/1, maps:to_list(Map)),
    Values = maps:values(Map),
    [{ip_vs_conn_status, _, syn_recv},
     {ip_vs_conn_status, _, fin_wait},
     {ip_vs_conn_status, _, time_wait},
     {ip_vs_conn_status, _, established}] = Values,
    ok.

test_server_wait(_Config) ->
    {ok, Map} = get_connections(),
    Keys = [{ip_vs_conn, tcp, syn_recv, 167792566, 69, 167792566, 8080, 167792566, 8081, 57},
            {ip_vs_conn, tcp, fin_wait, 167792566, 47808, 167792566, 8080, 167792566, 8081, 59},
            {ip_vs_conn, tcp, time_wait, 167792566, 47809, 167792566, 8080, 167792566, 8081, 59},
            {ip_vs_conn, tcp, established, 167792566, 62061, 167792566, 8080, 167792566, 8081, 58}],
    Keys = lists:map(fun ip_vs_conn:parse/1, maps:to_list(Map)),
    {ok, Map2} = get_connections(),
    Keys = lists:map(fun ip_vs_conn:parse/1, maps:to_list(Map2)),
    Map = Map2,
    Values = maps:values(Map),
    [{ip_vs_conn_status, _, syn_recv},
     {ip_vs_conn_status, _, fin_wait},
     {ip_vs_conn_status, _, time_wait},
     {ip_vs_conn_status, _, established}] = Values,
    ok.

test_update(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Conns = ip_vs_conn:fold(fun (C,L) -> [C | L] end, [], Proc),
    Start1 = erlang:monotonic_time(micro_seconds),
    Map = lists:foldl(fun ip_vs_conn_map:update/2, maps:new(), Conns),
    End1 = erlang:monotonic_time(micro_seconds),
    ct:pal("time to update empty ~p", [End1 - Start1]),

    Start2 = erlang:monotonic_time(micro_seconds),
    Map2 = lists:foldl(fun ip_vs_conn_map:update/2, maps:new(), Conns),
    End2 = erlang:monotonic_time(micro_seconds),
    ct:pal("time to update full ~p", [End2 - Start2]),
    Map = Map2,
    ok.


proc_file(Config, test_server2) -> ?config(data_dir, Config) ++ "proc_ip_vs_conn2";
proc_file(Config, test_server_wait) -> ?config(data_dir, Config) ++ "proc_ip_vs_conn2";
proc_file(Config, test_parse_large_close) -> ?config(data_dir, Config) ++ "ip_vs_conn_large_close";
proc_file(Config, test_parse_large_syn_recv) -> ?config(data_dir, Config) ++ "ip_vs_conn_large_syn_recv";
proc_file(Config, test_update) -> ?config(data_dir, Config) ++ "ip_vs_conn_large_syn_recv";
proc_file(Config, _) -> ?config(data_dir, Config) ++ "proc_ip_vs_conn".

init_per_testcase(Test, Config) ->
    ProcFile = proc_file(Config, Test),
    ct:pal("proc file is ~p", [ProcFile]),
    application:set_env(ip_vs_conn, proc_file, ProcFile),
    application:set_env(ip_vs_conn, interval_seconds, 1),
    application:set_env(ip_vs_conn, splay_seconds, 1),
    {ok, _} = application:ensure_all_started(ip_vs_conn),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(ip_vs_conn).


