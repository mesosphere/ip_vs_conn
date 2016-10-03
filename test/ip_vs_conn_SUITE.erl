-module(ip_vs_conn_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [test_parse].

test_parse(_Config) ->
  Root = "../../../../",
  Ret = [{ip_vs_conn,tcp,167792566,47808,167792566,8080,167792566,8081,syn_recv,59,[],[]},
         {ip_vs_conn,tcp,167792566,62061,167792566,8080,167792566,8081,syn_recv,58,[],[]},
         {ip_vs_conn,tcp,167792566,69,167792566,8080,167792566,8081,syn_recv,57,[],[]}],
  Ret = ip_vs_conn:parse(Root ++ "testdata/proc_ip_vs_conn"),
  ok.
