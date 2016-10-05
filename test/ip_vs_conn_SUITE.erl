-module(ip_vs_conn_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [test_gen_server,
          test_parse,
          test_parse_large_close,
          test_parse_large_syn_recv,
          test_parse_missing,
          test_server,
          test_server2,
          test_server_wait
         ].

test_parse(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Ret = [{ip_vs_conn,tcp,167792566,47808,167792566,8080,167792566,8081,syn_recv,59,[],[]},
           {ip_vs_conn,tcp,167792566,62061,167792566,8080,167792566,8081,syn_recv,58,[],[]},
           {ip_vs_conn,tcp,167792566,69,167792566,8080,167792566,8081,syn_recv,57,[],[]}],
    Ret = ip_vs_conn:parse(Proc),
    ok.

test_parse_large_close(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Start = erlang:monotonic_time(micro_seconds),
    Ret = ip_vs_conn:parse(Proc),
    End = erlang:monotonic_time(micro_seconds),
    ct:pal("time to parse ~p", [End - Start]),
    0 = length(Ret),
    ok.

test_parse_large_syn_recv(_Config) ->
    Proc = ip_vs_conn_config:proc_file(),
    Start = erlang:monotonic_time(micro_seconds),
    Ret = ip_vs_conn:parse(Proc),
    End = erlang:monotonic_time(micro_seconds),
    ct:pal("time to parse ~p", [End - Start]),
    65535 = length(Ret),
    ok.

test_parse_missing(_Config) ->
    [] = ip_vs_conn:parse("foobar"),
    ok.

test_gen_server(_Config) ->
    erlang:send(ip_vs_conn_monitor, hello),
    ok = gen_server:call(ip_vs_conn_monitor, hello),
    ok = gen_server:cast(ip_vs_conn_monitor, hello),
    sys:suspend(ip_vs_conn_monitor),
    sys:change_code(ip_vs_conn_monitor, random_old_vsn, ip_vs_conn_monitor, []),
    sys:resume(ip_vs_conn_monitor).

test_server(_Config) ->
    timer:sleep(2000),
    {ok, Map} = ip_vs_conn_monitor:get_dropped(),
    Keys = [{167792566,69,167792566,8080,167792566,8081},
            {167792566,47808,167792566,8080,167792566,8081},
            {167792566,62061,167792566,8080,167792566,8081}],
    Keys = maps:keys(Map),
    ok.

test_server2(_Config) ->
    timer:sleep(2000),
    {ok, Map} = ip_vs_conn_monitor:get_dropped(),
    Keys = [{167792566,69,167792566,8080,167792566,8081}],
    Keys = maps:keys(Map),
    ok.

test_server_wait(_Config) ->
    timer:sleep(2000),
    {ok, Map} = ip_vs_conn_monitor:get_dropped(),
    Keys = [{167792566,69,167792566,8080,167792566,8081}],
    Keys = maps:keys(Map),
    timer:sleep(2000),
    {ok, Map} = ip_vs_conn_monitor:get_dropped(),
    Keys = maps:keys(Map),
    ok.


proc_file(test_server2) -> "../../../../testdata/proc_ip_vs_conn2";
proc_file(test_server_wait) -> "../../../../testdata/proc_ip_vs_conn2";
proc_file(test_parse_large_close) -> "../../../../testdata/ip_vs_conn_large_close";
proc_file(test_parse_large_syn_recv) -> "../../../../testdata/ip_vs_conn_large_syn_recv";
proc_file(_) -> "../../../../testdata/proc_ip_vs_conn".

init_per_testcase(Test, Config) ->
    application:set_env(ip_vs_conn, proc_file, proc_file(Test)),
    application:set_env(ip_vs_conn, interval_seconds, 1),
    application:set_env(ip_vs_conn, splay_seconds, 1),
    {ok, _} = application:ensure_all_started(ip_vs_conn),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(ip_vs_conn).


