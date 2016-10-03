%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2015, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 02:32 PM
%%%-------------------------------------------------------------------
-module(ip_vs_config).
-author("Anatoly Yakovenko").

-export([interval_seconds/0,
         splay_seconds/0,
         ip_vs_conn_proc_file/0
         ]).

interval_seconds() ->
  application:get_env(ip_vs, interval_seconds, 60).

splay_seconds() ->
  application:get_env(ip_vs, splay_seconds, 10).
