%%%-------------------------------------------------------------------
%%% @author Anatoly Yakovenko
%%% @copyright (C) 2015, Mesosphere
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2016 11:44 AM
%%%-------------------------------------------------------------------
-module(ip_vs_monitor).
-author("Anatoly Yakovenko").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {
  callbacks = maps:new(),
  ip_vs_conn_syns = maps:new() %% all the connection that have seen a syn_recv
  }).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(init(term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:send_after(splay_ms(), self(), poll_proc),
  {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(poll_proc, State) ->
  State1 = update_ip_vs_conn(State),
  NewState = State1,
  erlang:send_after(splay_ms(), self(), poll_proc),
  {noreply, NewState};
   
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% TODO: borrowed from minuteman, should probably be a util somewhere
-spec(splay_ms() -> integer()).
splay_ms() ->
  MsPerMinute = ip_vs_config:interval_seconds() * 1000,
  NextMinute = -1 * erlang:monotonic_time(milli_seconds) rem MsPerMinute,

  SplayMS = ip_vs_config:splay_seconds() * 1000,
  FlooredSplayMS = max(1, SplayMS),
  Splay = rand:uniform(FlooredSplayMS),

  NextMinute + Splay.

update_ip_vs_conn(State) ->
  ProcFile = ip_vs_config:ip_vs_conn_path(),
  Conns = ip_vs_conn:parse(ProcFile),
  Syns = State#state.ip_vs_conn_syns,
  NewSyns = lists:fold(Conns,
                       fun(Conn, Acc) ->
                           update_ip_vs_syns(Syns, Acc, Conn)
                       end,
                       maps:new()),
  State#state{ip_vs_conn_syns = NewSyns}.

update_ip_vs_syns(Syns, Conn, Acc) ->
  Current = maps:get(conn_id(Conn), Syns),
  update_ip_vs_syns(Syns, Conn, Acc, Current).

%% new connection
update_ip_vs_syns(Syns, Conn#ip_vs_conn{tcp_state = syn_recv}, Acc, {badmap, _}) ->
  maps:put(conn_id(Conn), erlang:monotonic_time(seconds), Acc);

%% old connection still in syn_recv state
update_ip_vs_syns(Syns, Conn#ip_vs_conn{tcp_state = syn_recv}, Acc, Start) ->
  maps:put(conn_id(Conn), Start, Acc).

conn_id(Conn) ->
  {Conn#ip_vs_conn.from_ip,
   Conn#ip_vs_conn.from_port,
   Conn#ip_vs_conn.to_ip,
   Conn#ip_vs_conn.to_port,
   Conn#ip_vs_conn.dst_ip,
   Conn#ip_vs_conn.dst_port}.


