%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2015, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 02:32 PM
%%%-------------------------------------------------------------------
-module(ip_vs_conn_config).
-author("Anatoly Yakovenko").

-export([proc_file/0]).

proc_file() ->
  application:get_env(ip_vs_conn, proc_file, "/proc/net/ip_vs_conn").
